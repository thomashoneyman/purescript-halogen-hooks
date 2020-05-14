# The Hooks API

The Hooks API consists of 6 primitive Hooks you can use to implement components or custom Hooks.

This chapter is still a work in progress, but it contains enough information on each primitive Hook for you to get started using them in your own code.

## Table of Contents

1. [useState](#usestate)
2. [useLifecycleEffect](#uselifecycleeffect)
3. [useTickEffect](#usetickeffect)
4. [useQuery](#usequery)
5. [useMemo](#usememo)
6. [useRef](#useref)

## useState

The `useState` Hook allows you to create an independent state. It requires the initial state as an argument, and it returns the current value of that state and a unique identifier you can use with state functions to update that state.

```purs
Hooks.do
  -- Create one or more states with `useState`. On each render the hook will
  -- return the current state and a unique identifier you can use to update
  -- the state.
  state /\ stateId <- Hooks.useState initialState
  count /\ countId <- Hooks.useState 0

  let
    update :: HookM _ Unit
    update = do
      -- Use the modify function to update the state, which will cause all hooks
      -- to run again and a new render to occur.
      Hooks.modify_ countId (_ + 10)
      -- ...

  Hooks.useLifecycleEffect do
    Hooks.modify_ countId (_ + 10)
    pure Nothing

  Hooks.pure do
    HH.div
      [ HE.onClick \_ -> Just update ]
      [ HH.text $ show count ] -- Use state values directly in your render code
```

In a regular Halogen component, any time your state updates your component will re-render. Hooks operate in a similar fashion: any time one of your state cells updates, your Hooks will re-run.

Most of the time you only need the `modify_` function for your state. If you prefer the `useState` hook to just return the modify function directly, you can do so like this:

```purs
-- To allow using any state function
useStateFn :: forall s m a. (StateId s -> a) -> s -> Hook m (UseState s) (s /\ a)
useStateFn fn initial = map (map fn) (Hooks.useState initial)

-- To specifically use `modify_`
useState :: forall s m. s -> Hook m (UseState s) (s /\ ((s -> s) -> HookM m Unit)
useState = useStateFn Hooks.modify_

Hooks.do
  state /\ modifyState <- useState initialState

  let
    handler :: HookM _ Unit
    handler = do
      modifyState \st -> ...
```

## useLifecycleEffect

The `useLifecycleEffect` Hook allows you to run an effect the first time your Hooks are run, similar to a component initializer. This effect can optionally return another effect which will run when your Hooks are finalized. This second effect should be used to perform any necessary cleanup, like removing event listeners.

This Hook is useful when you need to perform effects which are not driven by user interactions, like loading resources, starting subscriptions and timers, and more. As with all effects in Hooks this code will run in the `HookM` monad.

If you would like to run your effect after every render, not just the initializer and finalizer, please see `useTickEffect`.

```purs
Hooks.do
  width /\ modifyWidth <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    -- This code will all be run after the first render, which is akin to
    -- component initialization.
    let readWidth = modifyWidth <<< const <<< Just <=< liftEffect <<< Window.innerWidth

    window <- liftEffect HTML.window
    subscriptionId <- Hooks.subscribe do
      ES.eventListenerEventSource
        (EventType "resize")
        (Window.toEventTarget window)
        (Event.target >>> map (fromEventTarget >>> readWidth))

    readWidth window

    -- This effect can return another effect to run when these Hooks are removed;
    -- here, we unsubscribe from window resize events.
    pure $ Just $ Hooks.unsubscribe subscription

  Hooks.pure width
```

Note: Asynchronous functions (functions defined during one Hooks evaluation, but run after another) should not reference `state` or `input` directly. Instead, any state or input they need access to should be copied into a mutable reference so that the function can read the reference when it runs, guaranteeing it has up-to-date values. In ordinary Hooks usage this mainly applies to the effect cleanup functions.

For a convenient Hook which does this for you, see the `useGet` Hook in the [examples](../examples/Example/Hooks).

## useTickEffect

The `useTickEffect` Hook lets you run an effect after every render, including the first time your Hooks are run, and optionally return another effect to run after the last time your Hooks are run. This second effect should be used to clean up any resources acquired with prior effects.

This Hook is used the largely the same way as the `useLifecycleEffect` Hook. However, because it runs after every render, there is more potential for it to become a performance bottleneck.

For that reason, this Hook is designed to only run again if particular values it depends on have changed. You must provide these dependencies via the `Hooks.captures` or `Hooks.capturesWith` functions. For example:

```purs
-- This effect will run after every render
Hooks.captures {} Hooks.useTickEffect do
  -- ... your effect body
  pure Nothing -- ... if no cleanup is required before the next run of the effect
  pure $ Just do -- ... if cleanup is required before the next run of the effect

-- This effect will run after the first render and after any render in which the
-- values `memoA` or `memoB` have changed:
Hooks.captures { memoA, memoB } Hooks.useTickEffect do
  -- ... your effect, which depends on memoA or memoB
  pure Nothing
```

It is easy to forget to include dependencies. If you forget a dependency, and then its value changes, then your effect will not re-run even though it should. To avoid this situation I recommend defining all code which relies on `captures` inside a `where` block. This prevents you from inadvertently using values in your effect which are in scope in your Hooks block.

```purs
Hooks.do
  count /\ _ <- Hooks.useState 0
  size /\ _ <- Hooks.useState 100.0
  _ <- useMyEffect { count, size }
  -- ... rest of your implementation
  where
  -- this code cannot accidentally forget to include `count` or `size` in its
  -- dependencies because they are not in scope.
  useMyEffect deps@{ count, size } = Hooks.captures deps Hooks.useTickEffect do
    -- ... use count and size
    pure Nothing
```

## useQuery

The `useQuery` Hook enables you to write components which can receive and evaluate queries from a parent component. This Hook is only usable in components constructed with the `Hooks.component` function, because the request/response nature of queries means they only make sense within components. Queries don't make sense in arbitrary Hooks, so they're disallowed.

The resulting component can be queried like any other Halogen component via the `Hooks.query` or `Hooks.queryAll` functions.

If this Hook is used multiple times, then only the last use will take effect.

```purs
data Query a = IsOn (Boolean -> a)

component :: forall i o m. H.Component HH.HTML Query i o m
component = Hooks.component \{ queryToken } _ -> Hooks.do
  enabled /\ modifyEnabled <- Hooks.useState false

  -- You can only use the useQuery Hook with a token, which must come from the
  -- `component` function
  Hooks.useQuery queryToken case _ of
    -- You can write a handler the same way you would write `handleQuery` in a
    -- Halogen component. The handler is updated on each Hooks evaluation, so
    -- you can refer to state or input values directly without them becoming stale.
    IsOn reply -> do
      pure (Just (reply enabled))

  Hooks.pure -- ...your render code
```

## useMemo

The `useMemo` Hook lets you preserve a value between runs of your Hooks, so long as the values used to compute it have not changed. This Hook is purely a performance optimization.

When you define values in let bindings in the body of a Hook they will be redefined each time the Hook's body is evaluated, which is to say on every render. For many values this doesn't matter, but for values that are expensive to compute this can become a performance bottleneck.

This is the same situation as exists in Halogen components: you should avoid computing expensive values within your render function. Instead, you should define these values outside the component, or if the values can be updated only within effectful code, then you can try caching them in state so re-renders don't require re-computing them.

However, sometimes you will have to compute an expensive value within the body of your Hooks code. In this case, you can use the `useMemo` hook to only recompute the value if a value it depends on has changed.

All values used to compute the value you want to memoize must be provided to the `Hooks.captures` or `Hooks.capturesWith` function. As with `useTickEffect`, you should define your `useMemo` Hook in a `where` clause so you don't use values that are in scope in your Hooks block without including them as a dependency.

```purs
-- this value will be computed on every render:
Hooks.do
  x /\ _ <- Hooks.useState 0
  y /\ _ <- Hooks.useState ""
  let expensiveValue = expensiveFunction x y
  -- ...

-- this value will only be computed if `x` or `y` have changed in the last render
Hooks.do
  x /\ _ <- Hooks.useState 0
  y /\ _ <- Hooks.useState ""
  expensiveValue <- useExpensive { x, y }
  -- ...
  where
  useExpensive deps@{ x, y } = Hooks.captures deps $ flip Hooks.useMemo \_ ->
    expensiveFunction x y
```

## useRef

The `useRef` Hook lets you use a mutable reference in the body of a Hook. The Hook returns the value of the reference at the time the Hook was run, and the reference itself you can use to manipulate the value.

As with all state, you should only use the returned value in rendering code or as a return value from your Hook. If you are using the value in effectful code, then you should always use `Ref.read` on the reference to get the current value at the time your effect is run. Otherwise you run the risk of stale state.

Mutable references should be used sparingly, but they are necessary to manage values which change often but do not relate directly to rendering (for example, running a debouncer).

```purs
import Effect.Ref as Ref

Hooks.do
  value /\ ref <- Hooks.useRef 0

  -- use the reference for reading and writing when in effectful code
  Hooks.captures {} Hooks.useTickEffect do
    current <- liftEffect $ Ref.read ref
    -- ... use the current value

  -- use the returned value when in pure code, like the render function
  Hooks.pure $ HH.text (show value)
```
