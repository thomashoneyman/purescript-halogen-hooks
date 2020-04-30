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

The `useState` Hook allows you to create an independent state. It requires the initial state as an argument, and it returns the current value of that state and a token usable with the state functions in `HookM` to update that state.

```purs
Hooks.do
  -- Create one or more states with `useState`
  state /\ stateToken <- Hooks.useState initialState
  intState /\ intStateToken <- Hooks.useState 0

  let
    update :: HookM _ Unit
    update = do
      -- Use `modify`, `modify_`, or `put` to modify the state, causing the
      -- hooks to all re-run.
      Hooks.modify_ intStateToken (_ + 10)
      -- ...

  Hooks.useLifecycleEffect do
    Hooks.modify_ intStateToken (_ + 10)
    pure $ Just $ do
      -- You should only use the state value returned by `useState` in code that
      -- is re-defined every time Hooks are evaluated (like `update` above). Here,
      -- though, we're writing an effect which will run when the component unmounts.
      --
      -- For that reason we should use `Hooks.get` to retrieve the value in state
      -- when this effect executes -- not necessarily the value that existed
      -- when this effect is defined.
      currentState <- Hooks.get stateToken
      -- ...

  Hooks.pure $ HH.div
    [ HE.onClick \_ -> Just update ]
    [ HH.text $ show intState ] -- Use state values directly in your render code
```

In a regular Halogen component, any time your state updates your component will re-render. Hooks operate in a similar fashion: any time one of your state cells updates, your Hooks will re-run.

**Note: the value returned by `useState` should only be used in code that is re-defined every time the Hook re-evaluates.** For example:

- Rendering code is the return value of the Hook and is therefore re-evaluated each time the Hook runs. You can use the state value directly.
- Effects defined in a `let` block, like `update` in the example above, will be re-defined each time the Hook runs. You can use the state value directly.
- Effects defined using `useEffect` or `useLifecycleEffect` may not be re-defined each time Hooks evaluate. You should use `Hooks.get` to retrieve the current state, and you shouldn't use the value returned by `useState` (it could be stale).

## useLifecycleEffect

The `useLifecycleEffect` Hook allows you to run an effect the first time your Hooks are run, similar to a component initializer. This effect can optionally return another effect which will run when your Hooks are finalized. This second effect should be used to perform any necessary cleanup, like removing event listeners.

This Hook is useful when you need to perform effects which are not driven by user interactions, like loading resources, starting subscriptions and timers, and more. As with all effects in Hooks this code will run in the `HookM` monad.

If you would like to run your effect after every render, not just the initializer and finalizer, please see `useTickEffect`.

```purs
Hooks.do
  width /\ widthState <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    -- This code will all be run after the first render, which is akin to
    -- component initialization.
    let readWidth = Hooks.put widthState <<< Just <=< liftEffect <<< Window.innerWidth

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

Remember that if you access state values within `useLifecycleEffect` or `useTickEffect` you should use `Hooks.get` to get the current state instead of using the value returned by `useState`. Otherwise, the effect you define will always refer to the value that existed in state when the effect was defined, which may have changed by the time your effect is executing.

## useTickEffect

The `useTickEffect` Hook lets you run an effect after every render, including the first time your Hooks are run, and optionally return another effect to run after the last time your Hooks are run. This second effect should be used to clean up any resources acquired with prior effects.

This Hook is used the largely the same way as the `useLifecycleEffect` Hook. However, because it runs after every render, there is more potential for it to become a performance bottleneck.

For that reason, this Hook is designed to only run again if particular values it depends on have changed. You must provide these dependencies via the `Hooks.captures` or `Hooks.capturesWith` functions. For example:

```purs
-- This effect will run after every render
Hooks.captures {} Hooks.useTickEffect do
  -- ... your effect

-- This effect will run after the first render and after any render in which the
-- values `memoA` or `memoB` have changed:
Hooks.captures { memoA, memoB } Hooks.useTickEffect do
  -- ... your effect, which depends on memoA or memoB
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

The resulting component can be queried like any other Halogen component via the `H.query` or `H.queryAll` functions.

If this Hook is used multiple times, then only the last use will take effect.

```purs
data Query a = IsOn (Boolean -> a)

component :: forall i o m. H.Component HH.HTML Query i o m
component = Hooks.component \{ queryToken } _ -> Hooks.do
  enabled /\ enabledState <- Hooks.useState false

  -- You can only use the useQuery Hook with a token, which must come from the
  -- `component` function
  Hooks.useQuery queryToken case _ of
    -- You can write a handler the same way you would write `handleQuery` in a
    -- Halogen component
    IsOn reply -> do
      -- This query handler won't run during the same evaluation it is defined
      -- in; it will run in response to query events. For that reason, you should
      -- always use `Hooks.get` to retrieve the state in a query handler, and
      -- not use the value returned by `useState`.
      isEnabled <- Hooks.get enabledState
      pure (Just (reply isEnabled))

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

  Hooks.pure $ HH.text (show value)
```
