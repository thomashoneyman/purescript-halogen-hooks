# Hooks at a Glance

Hooks are a new library for Halogen. This page provides an overview of using Hooks for experienced Halogen users. If you haven't read it yet, you should [read Introducing Halogen Hooks](https://thomashoneyman.com/articles/introducing-halogen-hooks) to understand the motivation for Hooks. You may also be interested in the Hooks recipes in the [PureScript Cookbook](https://github.com/JordanMartinez/purescript-cookbook) to see some common tasks implemented with Hooks.

This is a fast-paced overview. If you're new to Halogen you should take time to get familiar with essential Halogen concepts like input, state, and `HalogenM` before you read this.

## No Breaking Changes

One more thing: before we move on, I'd like to note that Hooks are:

- **Built on Halogen**. Hooks are implemented on top of Halogen, with no breaking changes to the underlying library.
- **Incrementaly adoptable**. You can begin using Halogen Hooks in a few components without rewriting existing code. You can adopt Hooks as quickly or as slowly, as completely or as piecemeal as you would like.
- **Compatible with components**. Halogen is based on components, and components are here to stay. Hooks are a more convenient way to define stateful logic in your applications, and they are sufficient to define components as well, but they don't remove the need for components.

Hooks provide a more direct API to the Halogen concepts you already know: input, state, queries, lifecycles, and side effects. Hooks are also a more flexible way to combine these features without boilerplate.

## The State Hook

This example renders a counter. When you click the button, it increments the value:

```purs
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks

example = Hooks.component \_ _ -> Hooks.do
  -- Declare a new state variable, which we'll call "count"
  count /\ countId <- Hooks.useState 0

  Hooks.pure do
    HH.div_
      [ HH.p_ [ HH.text $ "You clicked " <> show count <> " times" ]
      , HH.button
          [ HE.onClick \_ -> Just $ Hooks.modify_ countId (_ + 1) ]
          [ HH.text "Click me" ]
      ]
```

`useState` is a Hook. We call it inside a Hooks function to add some local state to it. Halogen will preserve this state between re-renders. `useState` returns a tuple: the _current_ state value and a unique identifier you can use with state update functions including `get`, `put`, `modify_`, and `modify`.

The update function runs in the `HookM` monad, which is also the action type that all HTML functions in Hooks use. That means you can use this update function directly in your HTML, or combine it with any other `HookM` code. We'll see how to use this monad later in this page, but it also has [its own documentation section](./05-HookM.md) if you're ready for a deep dive.

The `useState` hook requires an initial state as its only argument. In this example, the initial state starts our counter at `0`. The initial state argument is only used on the first render.

### Declaring multiple states

You can use the State Hook more than once in a single component:

```purs
manyStates = Hooks.do
  -- Declare multiple state variables!
  age /\ ageId <- Hooks.useState 42
  fruit /\ fruitId <- Hooks.useState "banana"
  todos /\ todosId <- Hooks.useState [ { text: "Learn Hooks" } ]
  -- ...
```

We're destructuring the tuple that `useState` returns using the tuple operator `(/\)`, which allows us to name our state value and identifier whatever we want.

### Using a modify function instead of an identifier

If you prefer your `useState` Hook to return a modify function directly, instead of an identifier, you can use the `Functor` instance for Hooks to apply a state function to the identifier returned by the hook as seen in the example below (`useStateFn`, as well as variants like `useModifyState`, are available in the [halogen-hooks-extra](https://github.com/JordanMartinez/purescript-halogen-hooks-extra) package).

```purs
-- You can provide any of the Hooks state functions to this function.
useStateFn :: forall s m a. (StateId s -> a) -> s -> Hook m (UseState s) (s /\ a)
useStateFn fn initial = map (map fn) (Hooks.useState initial)

manyStates = Hooks.do
  -- Return a modify function instead of an identifier!
  age /\ modifyAge <- useStateFn Hooks.modify_ 42
  fruit /\ setFruit <- useStateFn Hooks.put "banana"

  let
    handler :: HookM _ Unit
    handler = do
      -- instead of Hooks.modify_ ageId \n -> n + 10
      modifyAge \n -> n + 10
      -- instead of Hooks.put fruitId "strawberry"
      setFruit "strawberry"
```

### What is a Hook?

Hooks are functions that let you "hook into" Halogen state and lifecycle features without a component. Hooks ultimately must be run by a component; Halogen has no concept of Hooks built in to the library. You can turn a Hook which returns `ComponentHTML` (as our examples so far have done) into a component using the `Hooks.component` function.

Halogen Hooks provides a few built-in Hooks like `useState`. You can also implement your own Hooks to reuse stateful behavior among different components.

## The Effect Hook

Requests, subscriptions, and queries are all examples of side effects that components run all the time. In a regular Halogen component these effects must be run in `HalogenM`, usually as part of the `handleAction` or `handleQuery` functions used to construct the component. They can't be run during rendering.

In a Hooks function you write side effects in `HookM`, a monad almost identical to `HalogenM`. You can run these side effects in the body of the hook with `useLifecycleEffect` or `useTickEffect`, two implementations of the Effect Hook. This hook replaces component initializers and finalizers and also introduces the ability to run an effect after every render, not just the first and last one.

For example, this component will log the current count every time the state updates (ie. a render occurs):

```purs
example = Hooks.component \_ _ -> Hooks.do
  count /\ countId <- Hooks.useState 0

  -- On initialize and each subsequent render, log the current count state to
  -- the console.
  Hooks.captures {} Hooks.useTickEffect do
    Console.logShow count
    -- Before each run of the effect we can perform some cleanup (for example:
    -- ending a subscription, or cleaning up an event handler). Here, we don't
    -- need to, so we return `Nothing`.
    pure Nothing

  Hooks.pure do
    HH.div_
      [ HH.p_ [ HH.text $ "You clicked " <> show count <> " times" ]
      , HH.button
          [ HE.onClick \_ -> Just $ Hooks.modify_ countId (_ + 1) ]
      ]
```

The two implementations of this Hook differ in important ways:

- `useLifecycleEffect` will run after the first render (initialization) and can return an effect to run when the component is finalizing. It won't be run for subsequent renders in between these two. That means it directly replaces initializers and finalizers in regular components.
- `useTickEffect` will run after every render, including initialization. Some effects, like subscriptions to a data source provided as input or in state, need to update any time that source changes, not just at component initialization.

## Building Your Own Hooks

We often want to reuse some stateful logic between components. Traditionally, there were two solutions to this problem: higher-order and renderless components. Custom Hooks let you share stateful logic without the boilerplate and complexity of these patterns and without adding more components to your tree.

See [Implementing UseWindowWidth](https://thomashoneyman.com/articles/introducing-halogen-hooks/#implementing-usewindowwidth) for information on implementing a custom Hook of your own.

## Other Hooks

There are other built-in Hooks you may find useful. For example, you can use the `useRef` hook to acquire a reference to mutable state, `useQuery` to enable a request/response pattern for your component, or `useMemo` to memoize expensive values in your render code. You can see a full listing in the [Hooks API reference](./07-Hooks-API.md).

## Hooks and Ordering

Hooks are functions, but they're implemented with an indexed monad that ensures that Hooks are always called in the same order. You will receive a compile-time error if you try to call Hooks out of order (for instance, by calling Hooks within branching logic like `if` or `case` statements).

This restriction exists because Hooks are internally implemented in a Halogen component which stores the Hooks and their data in arrays. If Hooks are run out of order, then their array indices will no longer match, potentially causing a runtime crash. This implementation matches with the same approach used in React and React Basic Hooks.

In practice, it's rare to encounter this compile-time error. However, it's good to know that the restriction exists in case you do find yourself trying to order Hooks contingently.
