# Hooks Internals

If you are interested in contributing to the Halogen Hooks library then this documentation is for you! It explains, at a high level, the major concepts behind the Hooks implementation. This can help you understand the big picture when you're reading a specific part of the code base. However, this isn't reference documentation -- you'll still need to read the underlying code to see the specifics.

This documentation assumes you have already read the preceding chapters and understand the library as a user.

## What Are Hooks?

Hooks are a simpler mental model for writing stateful logic in Halogen. They allow you to express stateful logic without rendering and share those stateful functions among many components. You can even nest Hooks arbitrarily deep within other Hooks.

Hooks is a library built on top of Halogen and Hooks components can be freely intermixed with ordinary Halogen components. Ultimately, Hooks are "compiled down to" or "interpreted by" a regular Halogen component which you can use in your code base like any other Halogen component.

## Major Implementation Concepts

Hooks are implemented as two free monads which can be interpreted into `HalogenM` in the context of a Halogen component. (They can also be interpreted differently for testing purposes.) Over the next few sections we'll break that sentence apart according to the implementation.

### Hooks As a Pair of Free Monads

Hooks are implemented as a pair of free monads.

The first free monad is `Hook`, built from the `UseHookF` algebra. This monad provides `useState`, `useTickEffect`, and other primitive hooks. It is the monad used to opt-in to features like state, effects, memoization, and mutable references. Each call to a constructor like `UseState` or `UseEffect` is interpreted to an equivalent behavior in `HalogenM` like a call to modify the underlying component state.

This monad is interpreted into `HalogenM` by the `evalHook` function.

```text
Hook --------> evalHook --------> HalogenM
```

The second free monad is `HookM`, built from the `HookF` algebra. This monad provides all the same functions you are used to from `HalogenM`, tweaked for compatibility with Hooks. For example: you can create multiple independent states with Hooks, so the `modify` function needs to take an additional argument identifying _which_ state you want to modify.

This monad is interpreted into `HalogenM` by the `evalHookM` function.

```text
HookM -------> evalHookM ------> HalogenM
```

These monads are mutually-dependent. For example:

- When you register an effect with `useTickEffect`, the effect itself is of type `HookM m Unit`. Therefore, to actually evaluate the effect you need to be able to evaluate `HookM` to `HalogenM`.
- Hooks are always run before each render, and a render always happens after a state update. Therefore, to use a `HookM` function like `Hooks.modify` to update state, you also need to evaluate the Hooks (`Hook` to `HalogenM`) before rendering.

These two free monads (`Hook` and `HookM`) and their interpreters (`evalHook` and `evalHookM`) form the core of the Hooks library. But we are also going to need a component which can evaluate the resulting `HalogenM` code.

### Executing Hooks in a Component

The two free monads `Hook` and `HookM` are both interpreted into `HalogenM`. More specifically, they're interpreted into [this full type](https://github.com/thomashoneyman/purescript-halogen-hooks/blob/c3b4730f9a5e17dcc3f50d2629bed6d09c34a680/src/Halogen/Hooks/Internal/Eval/Types.purs#L15).

As indicated by this type, the underlying component which executes Hooks will:

- Maintain a single internal state for Hooks, which handles all the bookkeeping for tracking what states have been created, holding memoized values, queueing effects to run post-render, and so on.
- Use `HookM m Unit` as its action type, which means that any `ComponentHTML` rendered by this component must also use `HookM` as its action type and that the `handleAction` function must evaluate the `HookM` code.
- Use opaque types to represent public parts of its interface, namely querying child components and raising output messages (more on this later).
- Allow execution in some monad `m` and return some value `a`, just like ordinary `HalogenM` code.

Components are constructed from a record of three parts: `{ initialState, render, eval }`. Over the next three sections we'll discuss how the underlying Hooks evaluation component handles state, rendering, and evaluation.

#### State

Users of the Hooks library can write simple stateful functions which accept some arguments, opt-in to features like state and effects, and produce some result. This requires some bookkeeping internally. We need to:

1. Keep track of the Hooks that the user has used, including any data those Hooks rely on
2. Keep track of the input to the Hooks function over the lifetime of the component
3. Keep track of what effects (if any) to evaluate after each render

The underlying component will create this initial bookkeeping state. Then, it will update this state when evaluating the `Hook` and `HookM` free monads and return portions of this state to those monads when asked.

Here's [the internal state](https://github.com/thomashoneyman/purescript-halogen-hooks/blob/c3b4730f9a5e17dcc3f50d2629bed6d09c34a680/src/Halogen/Hooks/Internal/Eval/Types.purs#L48-L65) used in Hooks.

We can break down some of its major parts:

1. We store the result of the Hook -- in the context of the component this is always `ComponentHTML`
2. We store the internal bookkeeping state, which includes:
   2a. The input to the component so we can provide it to the Hooks function
   2b. An array for each type of Hook so we can store its data
   2c. A queue of effects we will fill each time we interpret our Hooks function so we can execute them after we've rendered.

The most notable part of this state are the set of arrays used to track data about each Hook.

The first time we interpret a Hooks function every bind writes a new "cell" into one of these arrays. For example, using `useState` will insert the user's initial state at the end of the state array. Then, on all subsequent evaluations of the Hooks function, we will read those indices in order. For example, if we wrote this Hook:

```purs
_ <- useState 0
useTickEffect do ...
_ <- useState ""
```

...then on the first Hooks evaluation we would insert `0` in the state array, then a `HookM` effect in the effects array, and then `""` in the state array. On each subsequent evaluation we would read the state at the first index in the state array, the effect at the first index in the effects array, and then the state at the second index in the state array.

#### Enforcing Safety With Indexed Free Monads

This is a good time to revisit our first free monad, `Hook`. As you have noticed as a user of the library this is not a regular free monad. Instead, it's an _indexed_ free monad, which means that you can track the state before and after a bind in the type:

```purs
type Hook m (newHook :: Type -> Type) a = forall hooks. Hooked m hooks (newHook hooks) a

newtype Hooked m pre post a = Hooked (Indexed (Free (UseHookF m)) pre post a)
```

The hook type tracked by this indexed free monad ensures that there is only one possible sequence of binds when it is evaluated. In other words, there are no "forks in the road": you can't use conditionals to decide what code to evaluate. You must always evaluate the same Hooks in the same order, enforced by the type.

Why does this matter?

As we saw in the previous section, Hooks are initialized _once_ and each `Hook` stores some state in an array in the underlying component state. The State hook stores states, the Effect hooks store memoized values and effects to run, the Memo hook stores memoized data, and so on. When Hooks are interpreted via `evalHook` we access each of these arrays in turn, under the assumption that **the same Hooks are running in the same order as they were originally initialized.**

It is only safe to perform this series of array accesses when the number and order of `Hooks` we are using does not change. Otherwise we can end up accessing incorrect data or crashing altogether. For example:

```purs
if x > 1 then
  a /\ _ <- useState 0
  pure a
else
  a /\ _ <- useState 0
  b /\ _ <- useState 0
  pure (a + b)
```

If we evaluate the first branch then our component state will contain a state array with a single entry. If we then evaluate the second branch our application will crash: the second call to `useState` will try to access the index `1` in the underlying state array, which does not exist.

Our internal implementation is safe because the indexed free monad ensures cases like this are impossible.

#### Rendering

The first field in the underlying Hooks state holds the result of the Hook. When interpreting Hooks via the `component` function this result will always be `H.ComponentHTML`. Only a Hook which returns `ComponentHTML` can be turned into a component because components are required to render something.

Rendering in the Hooks component is quite simple: after each Hooks evaluation we retrieve the resulting `ComponentHTML` and set it in component state. To render we simply retrieve the `ComponentHTML` from state.

#### Evaluation

We've now discussed the state and render function used by the Hooks component. Next, we can turn to the `eval` function to understand how this component can execute users' Hooks code.

The standard Halogen eval function maps several constructors to `HalogenM`:

```purs
eval :: HalogenQ query action input ~> HalogenM state action slots output m

data HalogenQ query action input a
  = Initialize a
  | Finalize a
  | Receive input a
  | Action action a
  | Query (Coyoneda query a) (Unit -> a)
```

Our `eval` function maps each of these constructors to our specific `HalogenM` type. Every time Halogen invokes our `eval` function we will either run `evalHook` (when initializing, finalizing, or receiving new input) or `evalHookM` (when evaluating a query or action). Because these two evaluation functions are mutually-dependent, evaluating one may also require evaluating the other, and the result is always our specific `HalogenM` type for the Hooks component.

Let's make that concept more concrete by describing what specifically happens in each case:

1. When the component initializes (the `Initialize` constructor), we create our initial state and call `evalHook` to evaluate our Hooks to `HalogenM` for the first time. This will fill the arrays in the underlying state with the bookkeeping details needed for subsequent Hooks evaluations.
2. When the component finalizes (the `Finalize` constructor), we call `evalHook` for the last time, running any finalizers provided via Hooks like `useLifecycleEffect`.
3. When the component receives new input (the `Input` constructor), we update our internal state to store the new input, call `evalHook`, and render.
4. When the component receives an action to run, we know it must be some `HookM` code (as that's the action type for our component). We therefore evaluate our `HookM` code via `evalHookM`.
5. When the component receives a query to run, we evaluate the `HookM` code the user has provided to run for that query and return the result to the parent component.

#### Component Tokens

The `component` and `memoComponent` functions allow you to turn a Hook which produces takes some input and produces `ComponentHTML` into a regular Halogen component. However, some features of Halogen do not make sense to be used in a Hook other than one which has been turned into a component. Queries, output messages, and child component slots are all features which are only relevant when you're using a component and should not be used in an arbitrary Hook.

To prevent users from trying to use these features in arbitrary Hooks code they require _tokens_. These tokens are only produced by via a the `component` functions and represent the component's query, output, and slot types. They are implemented as data types with no inhabitants -- there is no actual value which corresponds with a token and they are produced by coercing the unit value (an empty record).

These tokens are passed through to the Hook which the component is executing. That Hook can use:

1. The `OutputToken` value in calls to `raise` to send an output message
2. The `QueryToken` value in calls to the `useQuery` Hook to respond to queries from a parent
3. The `SlotToken` value in callso to `query` and `queryAll`, to query child components.

These tokens enable safe use of component features in Hooks code.
