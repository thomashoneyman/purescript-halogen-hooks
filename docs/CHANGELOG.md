# Changelog

Notable changes to Hooks are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

- **Move to a single index for hook types ([#32](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/32)).**

  Using a single index for Hook types simplifies the Hooks implementation and makes defining your own Hooks easier and less confusing. It also allows the library to drop its dependency on `indexed-monad`.

  Previously, Hooks were defined as an indexed monad with parameters to track the before and after state of a Hook bind:

  ```purs
  -- Old approach
  type Hook m (newHooks :: Type -> Type) a =
    forall hooks. Hooked m hooks (newHooks hooks) a

  newtype Hooked m hooks newHooks a =
    Hooked (Indexed (Free (UseHookF m)) hooks newHooks a)

  bind
    :: forall a b x y z m
    . Hooked m hooks hooks' a
    -> (a -> Hooked m hooks' newHooks b)
    -> Hooked m hooks newHooks b
  ```

  Now, Hooks are defined with a custom `HookType` and each bind produces a new, single index made up of all the hook types in use.

  ```purs
  -- New approach
  foreign import kind HookType

  newtype Hook m (h :: HookType) a = Hook (Free (UseHookF m) a)

  foreign import data Hooked :: HookType -> HookType -> HookType
  infixr 1 type Hooked as <>

  bind
    :: forall h h' m a b
    . Hook m h a
    -> (a -> Hook m h' b)
    -> Hook m (h <> h') b
  ```

  This is a breaking change because it changes how you define your Hook types in Halogen Hooks. First, Hooks are now written in the order they occur.

  ```purs
  -- Assume UseState, then UseEffect, then UseRef in the code

  -- Previously: this reads backwards, as state transitions 'away from' the
  -- hooks type variable
  UseRef Int
    (UseEffect
    (UseState Int hooks))

  -- Now: this reads in the order hooks are applied in the code, where
  -- `Pure` represents the call to `pure`
  UseState Int
    <> UseEffect
    <> UseRef Int
    <> Hooks.Pure
  ```

  Second, you no longer use a newtype to define Hooks. Instead, you'll foreign import a data type to represent your Hook type and use the `HookNewtype` type class.

  ```purs
  -- Before
  type UseX' hooks = UseState Int (UseEffect hooks)

  newtype UseX hooks = UseX (UseX' hooks)

  derive instance newtypeUseX :: Newtype (UseX hooks) _

  -- After
  type UseX' = UseEffect <> UseState Int <> Hooks.Pure

  foreign import data UseX :: Hooks.HookType

  instance newtypeUseX :: HookEquals UseX' h => HookNewtype UseX h
  ```

New features:

Bugfixes:

Other improvements:

- Docs: Added technical documentation that covers the main concepts used in the internal implementation ([#59](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/59)).
- Docs: Added a changelog to record changes to the library over time ([#62](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/62)).
- Tests: Added performance tests to measure the performance impact of changes ([#53](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/53), [#56](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/56)).

## [0.4.3] - 2020-06-17

This release ensures that state-modifying `HookM` code can't be passed from one component to another without throwing an immediate exception. `HookM` code that modifies state which is written in one component must be evaluated in that component.

Bugfixes

- Throw exception if state-modifying `HookM` code passed between components ([#44](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/44)).

Other improvements

- Updates Spago package set and generated Bowerfile

## [0.4.1] - 2020-06-04

This release updates module exports.

Bugfixes

- Re-export `memoComponent` from the main `Hooks` module ([#43](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/43)).

## [0.4.1] - 2020-05-18

This release includes small internal performance improvements.

Improvements:

- **Use `substFree` instead of `foldFree` internally ([#33](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/33)).**
  Using `foldFree` is convenient, but it incurs some overhead due to a `MonadRec` constraint on the monad you interpret into. Switching to `substFree` eliminates this overhead, giving the library a modest performance improvement.

## [0.4.0] - 2020-05-14

This release changes how users update state in Hooks.

Breaking changes (😱!!!):

- **Return to state identifiers instead of returning just a modify function ([#31](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/31)).**

  The previous release replaced state tokens with a simply `modify` function returned by the `useState` hook. For a variety of reasons this turned out to be not a change worth making, and it has now been reverted. See #30 for more details on why this happened.

  If you liked using a modify function instead of a token, you can still do that:

  ```purs
  state /\ modifyState <- map Hooks.modify_ <$> Hooks.useState 0
  let handleClick = modifyState (_ + 1)
  Hooks.pure ...
  ```

## [0.3.0] - 2020-05-07

This release changes how users update state in Hooks.

Breaking changes (😱!!!):

- **Replace state tokens with a modify function ([#29](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/29)).**

  The previous versions of Hooks returned a state token from the `useState` hook, which could then be passed to the `put`, `modify`, `modify_`, and `get` functions we're all used to from HalogenM. Now, the `useState` hook returns a modify function directly which can be used to update the state. See #29 for more details on why this matters.

  This code from v0.2.1:

  ```purs
  state /\ stateToken <- Hooks.useState 0
  let handleClick = Hooks.modify_ stateToken (_ + 1)
  Hooks.pure ...
  ```

  Would now be written like this:

  ```purs
  state /\ modifyState <- Hooks.useState 0
  let handleClick = modifyState (_ + 1)
  Hooks.pure ...
  ```

  Now that there isn't a `get` function, if you need to get the most up-to-date state in an asynchronous function, you should copy the relevant part of state to a mutable reference so the function can read the reference during its execution. This is the same pattern you should use if you need to do the same with component input. #29 also introduces a `useGet` example Hook which makes this easy and convenient.

## [0.2.0] - 2020-04-30

This release fixes several bugs and changes some types used in the Halogen Hooks library.

### Breaking changes (😱!!!):

- **Introduce tokens for all component-only features, not just queries ([#22](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/22)).**

  Hooks support writing functions for stateful logic, which are then interpreted by a Halogen component. However, some component features do not make sense in the context of Hooks alone (queries, slot types, and output messages).

  The first version of Hooks made queries available in a Hook only via a query 'token', which was provided by the `componentWithQuery` function. This approach has been extended to slot types and output messages as well. With this change Hook types no longer carry around slot or output types, which cleans up type signatures for the vast majority of cases, but they are still able to support child components and sending messages once used with the `component` function.

  This includes several breaking changes, all of which are simple to adjust to the new version (no features have been removed). In summary, any Hook types that previously accepted a slot and output type will no longer have them, and any Hook functions that use these types will now use a token as their first argument. Here's the full list of changes:

  - `Hook ps o m hooks a` is now `Hook m hooks a`
  - `Hooked ps o m hooksPre hooksPost a` is now `Hooked m hooksPre hooksPost a`
  - `HookM ps o m a` is now `HookM m a`
  - The `component` function has been updated to take as its first argument a record containing the query token, slot token, and output token that can be used to enable component features in a Hook. Any usage of `component \input -> ...` can be replaced with `component \_ input -> ...`.
  - The `componentWithQuery` function has been removed, as it is now covered by `component`. Any usage of `componentWithQuery \queryToken _ -> ...` can be replaced by `component \{ queryToken } _ -> ...`.
  - The `HookM` function `raise` now takes as its first argument an `OutputToken`. Any use of `Hooks.raise output` can be replaced by `component \{ outputToken } _ -> ... Hooks.raise outputToken output`.
  - The `HookM` functions `query` and `queryAll` now take as their first argument a `SlotToken`. Any use of `Hooks.query ...` can be replaced by `component \{ slotToken } _ -> ... Hooks.query slotToken ...`.

Bugfixes:

- Memo values could get out of sync with their indices in state ([#11](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/11)).
- Effect cleanup for useTickEffect would not run until the component finalized, running all cleanup functions together at the end ([#12](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/12)).
- State changes triggered by effects would not cause hooks to be re-evaluated ([#20](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/20)).

Other improvements:

- Tests: add automated tests for Hooks ([#19](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/19)).
- Tests: add continuous integration to the repository via GitHub Actions.
- Docs: update the documentation to make it clear when getting state vs. using the state from `useState` is necessary ([#7](https://github.com/thomashoneyman/purescript-halogen-hooks/pull/17)).
- Docs: update all public documentation to use new types and remove mention of `componentWithQuery`.

## [0.1.0] - 2020-04-30

Initial release of the Halogen Hooks library.
