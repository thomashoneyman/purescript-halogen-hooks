# Hooks FAQ

This document is still a work in progress, but you may find some of your questions answered below. If not, please consider asking on this repository's issue tracker. I can then answer and add the question and answer to this chapter.

## Table of Contents

- [Adopting Hooks](#adopting-hooks)

  - [Do I need to rewrite my existing components to use this library?](#do-i-need-to-rewrite-my-existing-components-to-use-this-library)
  - [How much of my Halogen knowledge is still relevant?](#how-much-of-my-halogen-knowledge-is-still-relevant)
  - [Do Hooks replace higher-order and renderless components?](#do-hooks-replace-higher-order-and-renderless-components)

- [From Halogen to Hooks Components](#from-halogen-to-hooks-components)

  - [How do I render `ComponentHTML`?](#how-do-i-render-componenthtml)
  - [How do I receive input?](#how-do-i-receive-input)
  - [How do I update state?](#how-do-i-update-state)
  - [Why am I seeing stale input or state?](#why-am-i-seeing-stale-input-or-state)
  - [How do I use initializers and finalizers?](#how-do-i-use-initializers-and-finalizers)
  - [How do I use `HalogenM` functions like `raise` and `query`?](#how-do-i-use-halogenm-functions-like-raise-and-query)
  - [How do I use actions?](#how-do-i-use-actions)

- [Technical Choices](#technical-choices)

---

## Adopting Hooks

### Do I need to rewrite my existing components to use this library?

You don’t need to rewrite any components to begin using Halogen Hooks. Components written with Hooks are ordinary Halogen components, which means you can mix them with any other Halogen component in a tree. You just can’t use Hooks /within/ an ordinary Halogen component — you should just convert the component entirely to the Hooks style.

For more information, see the documentation on [useState](./02-State-Hook.md), [useLifecycleEffect and useTickEffect](./03-Effect-Hook.md), and [useQuery](./04-Query-Hook.md), which all show how to convert Halogen components to Hook components. You can also see the [Halogen section in the Hooks examples](./examples) which recreates example components from the official Halogen repository.

### How much of my Halogen knowledge is still relevant?

Hooks don’t invalidate your existing Halogen knowledge. They are a more direct way to use Halogen features like state, lifecycles, and effects, but they don’t change how Halogen works underneath. Hooks are best thought of as a nicer way to write components and especially to share stateful logic — but the result is still Halogen running an ordinary component. Your knowledge of components, inputs, queries, the render cycle, and the features available in `HalogenM` like subscriptions are all still relevant.

Hooks do have a learning curve, however. If you have a question, feel free to [open an issue](https://github.com/thomashoneyman/purescript-halogen-hooks/issues/new) and we can discuss the issue and how to improve the documentation.

### Do Hooks replace higher-order and renderless components?

Almost all higher-order and renderless components would be better written as custom Hooks. Hooks are explicitly designed for sharing stateful logic and require significantly less complexity and boilerplate to use as compared to these two other solutions. Hooks are still new, however, and if you find a case where a higher-order or renderless component can’t be converted to Hooks, please [open an issue](https://github.com/thomashoneyman/purescript-halogen-hooks/issues/new).

---

## From Halogen to Hooks Components

### How do I render `ComponentHTML`?

Hooks use the same `ComponentHTML` type used throughout Halogen. Your render functions can be adopted from your existing components largely as-is. For example, this is a perfectly valid Hooks-based component:

```purs
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.HTML as HH

button = Hooks.component \_ _ -> Hooks.pure do
  HH.button [] [ HH.text "Click me!" ]
```

The major difference between render code in Hooks and render code in a usual Halogen component is that Hooks code doesn't ordinarily use actions. Instead, you write your handlers directly in to your render code. See [How do I use actions?](#how-do-i-use-actions) for more.

### How do I receive input?

Hooks are stateful functions, and they can take arguments. For custom Hooks that you are not turning into components directly, just write them as functions with arguments like you would write any other PureScript function.

If you need to turn a Hook which renders `ComponentHTML` into a component which receives continuous input from a parent, then your Hook can only take a single argument. Its argument will become the component's `Input` type when used with `Hooks.component`.

For example, this Hooks-based component receives an integer as input from its parent. Each time the input changes this component will re-render.

```purs
type Input = Int

example :: forall q o m. H.Component HH.HTML q Input o m
example = Hooks.component \_ input -> Hooks.pure do
  HH.div_
    [ HH.text "My input value is: "
    , HH.strong_ [ HH.text $ show input ]
    ]
```

The resulting component is an ordinary Halogen component which can be provided input from a parent like any other.

In the Hooks model you don't copy your input into your component state. It's purely an argument to your stateful function.

### How do I update state?

In Halogen components you update state using functions available in the `HalogenM` monad, namely `get`, `put`, `modify`, and `modify_`. In Hooks, when you use the `useState` hook, you are returned a state value and a unique identifier for that state which you can use with `get`, `put`, `modify`, and `modify_`. You receive an identifier because Hooks can have multiple independent states instead of a single unified state like an ordinary Halogen component.

```purs
(state :: Int) /\ (stateId :: StateId Int) <- useState 0
```

You can use the state functions with the identifier anywhere you can use `HookM`, which includes actions in your `ComponentHTML` and effects in hooks like `useLifecycleEffect`.

If you prefer your `useState` hook to return a modify function directly, you can use the `Functor` instance to apply the state function you want to the identifier for the hook to return. For example:

```purs
useStateFn :: forall s m a. (StateId s -> a) -> s -> Hook m (UseState s) (s /\ a)
useStateFn fn initial = map (map fn) (Hooks.useState initial)

state /\ (setState :: Int -> HookM m Unit) <- useStateFn Hooks.put 0
state /\ (modifyState :: ((Int -> Int) -> HookM m Unit) <- useStateFn Hooks.modify_ 0
```

### Why am I seeing stale input or state?

If you define a function in one Hooks evaluation which is going to be run during or after another Hooks evaluation, and this function refers to an `input` value or a value returned by `useState`, then the function will probably see a stale value when it runs. That's because `input` and values returned by `useState` are not mutable references; when your function runs, it will still be pointing at the value that existed when it was defined.

The only times you need to worry about this are when:

- You are defining an effect cleanup, and it relies on the current state
- You are defining a forked computation that may run at any time, and it relies on the current state

Otherwise, you can use the `state` value returned by `useState` or the `input` value directly.

If you do find yourself with stale input or state then you have two solutions:

- Use the `Hooks.get` function with your state identifier within your effect cleanup or forked effect to retrieve fresh state at the time the function executes.
- Copy the relevant portion of input into a `Ref` to retrieve fresh input at the time the function executes

```purs
myComponent :: forall q o m. MonadAff m => H.Component HH.HTML q Int o m
myComponent = Hooks.component \_ input -> Hooks.do
  state /\ stateId <- Hooks.useState 0
  _ /\ inputRef <- Hooks.useRef input

  -- Every time the value of `input` changes, we'll write the value to the input
  -- ref so it can be read by asynchronous functions / effect cleanup.
  Hooks.captures { input } Hooks.useTickEffect do
    liftEffect $ Ref.write input ref
    pure Nothing

  Hooks.captures {} Hooks.useTickEffect do
    -- These references are up to date because this effect body runs immediately
    -- after the Hook evaluation in which it is defined.
    logShow state
    logShow input
    pure $ Just $ do
      -- The effect cleanup, however, will not run after the Hook evaluation in
      -- which it is defined. For that reason we cannot use `state` or `input`
      -- directly, and should instead call `get <stateId>` and read the input ref
      -- to get the current value at the time the function executes.
      state' <- Hooks.get stateId
      input' <- liftEffect $ Ref.read inputRef
      logShow state'
      logShow input'
```

### How do I use initializers and finalizers?

Initializers and finalizers are lifecycle events provided by components. You can directly translate your initializers and finalizers to Hooks with `useLifecycleEffect`. This Hook takes an effect to run the first time your Hooks are evaluated, which can return another effect to run when your Hooks are run for the last time.

This model is more convenient than traditional initializers and finalizers because values don't have to be persisted in state between the effects.

This code demonstrates subscribing to a resource the first time the Hook runs and cleaning up the subscription the last time the Hook runs.

```purs
example = Hooks.component \_ _ -> Hooks.do
  Hooks.useLifecycleEffect do
    subscriptionId <- Hooks.subscribe ...
    pure $ Just $ Hooks.unsubscribe subscriptionId
```

We didn't have to persist the subscription ID in state because the finalizer effect is implemented in the same scope as the initializer effect.

### How do I use `HalogenM` functions like `raise` and `query`?

`HookM` supports all functionality available in `HalogenM`, including raising messages, forking threads, starting and stopping subscriptions, querying child components, and more. All functions from `HalogenM` except for the state functions are also available in `HookM` under the same name:

```text
H.fork      -> Hooks.fork
H.subscribe -> Hooks.subscribe
...
```

The `HalogenM` state functions only allow a single state, but Hooks allow multiple independent states. For that reason, the `useState` hook returns a state and a state identifier. The state identifier can be used with the `put`, `modify`, `modify_`, and `get` functions -- the same ones from `HalogenM`, just with the identifier as an extra argument.

Because the returned state value is updated every Hooks evaluation, you only need to call `get` in asynchronous functions (effect cleanups and forked functions, typically). To avoid stale state in asynchronous functions, see the [Why am I seeing stale input or state?](#why-am-i-seeing-stale-input-or-state) entry.

```purs
HalogenM.put      -> Hooks.put <id>
HalogenM.modify   -> Hooks.modify <id>
HalogenM.modify_  -> Hooks.modify_ <id>
HalogenM.get      -> Hooks.get <id>
```

Functions which send queries or send output messages are the same as they are in `HalogenM`, but they take a token returned by the `component` function as an additional argument. That's because these features only make sense in the context of a parent-child component relationship, which doesn't exist in Hooks:

```purs
HalogenM.query     -> Hooks.query slotToken
HalogenM.queryAll  -> Hooks.queryAll slotToken
HalogenM.raise     -> Hooks.raise outputToken
```

### How do I use actions?

Hook components use `HookM` as their action type, so there is no longer a dedicated feature for managing actions. Instead, you have two options.

Let's demonstrate both approaches via rewriting simple Halogen component.

```purs
data Action = Click

handleAction :: forall state slots output m. Action -> HalogenM state Action slots output m Unit
handleAction = case _ of
  Click -> ...

myComponent :: forall q i o m. Halogen.Component q i o m
myComponent =
  H.mkComponent
    { initialState: identity
    , render: \_ -> HH.button [ HE.onClick \_ -> Just Click ] [ HH.text "Click me" ]
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
```

Our first option is to avoid `Action` types altogether; with Hooks we can simply include `HookM` code in our render code. It's essentially the same thing but without the layer of indirection of going through actions.

To rewrite this component we can translate our action into its `HookM` code directly, and call that in our `ComponentHTML`:

```purs
myComponent :: forall q i o m. Halogen.Component q i o m
myComponent = Hooks.component \_ _ -> Hooks.do
  let
    handleClick :: HookM m Unit
    handleClick = ...

  Hooks.pure $ HH.button [ HE.onClick \_ -> Just handleClick ] [ HH.text "Click me" ]
```

However, in components with complex logic you may still want an `Action` type and a single `handleAction` function so you can better see the logic grouped together. You can easily recreate this pattern in Hooks:

```purs
data Action = Click

handleAction :: forall m. Action -> HookM m Unit
handleAction = case _ of
  Click -> ...

myComponent :: forall q i o m. H.Component q i o m
myComponent = Hooks.component \_ _ -> Hooks.pure do
  HH.button [ HE.onClick \_ -> Just $ handleAction Click ] [ HH.text "Click me" ]
```

---

## Technical Choices

- How does Hooks code compile to a Halogen component?

- Why do Hooks have to be run in order?

- Why do custom Hooks have to use newtypes?

- How are Hooks evaluated on each render?
