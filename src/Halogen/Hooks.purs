-- | This module implements the entire Halogen Hooks library. It's implemented as
-- | a monolithic module so that only types and functions meant for users are
-- | exported.
module Halogen.Hooks
  ( -- Hook API
    useState
  , UseState
  , useLifecycleEffect
  , useTickEffect
  , UseEffect
  , useQuery
  , UseQuery
  , useMemo
  , UseMemo
  , useRef
  , UseRef

  -- Hook types and helpers
  , module Halogen.Hooks.Component
  , module Halogen.Hooks.Internal.Types
  , module Halogen.Hooks.UseHookF

  -- Helpers
  , captures
  , capturesWith
  , publish

  -- HookM, which supports the same functions as HalogenM
  , module Halogen.Hooks.HookM

  -- Qualified do
  , bind
  , discard
  , pure
  )
where

import Halogen.Hooks.HookM

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Free (liftF)
import Data.Eq (class Eq)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Ref (Ref)
import Halogen.Hooks.Internal.Types (MemoValues, QueryToken) -- Ensure only these 2 are exported
import Halogen.Hooks.Internal.Types as IT
import Halogen.Hooks.UseHookF (Hook, UseHookF(..), Hooked(..))
import Halogen.Hooks.Component (component, componentWithQuery)
import Prelude (Unit, unit, ($), (<<<), (==))
import Unsafe.Coerce (unsafeCoerce)

foreign import data UseState :: Type -> Type -> Type

useState
  :: forall state slots output m
   . state
  -> Hook slots output m (UseState state) (state /\ StateToken state)
useState initialState = Hooked $ Indexed $ liftF $ UseState initialState' interface
  where
  initialState' :: IT.StateValue
  initialState' = IT.toStateValue initialState

  interface
    :: { value :: IT.StateValue, token :: StateToken IT.StateValue }
    -> state /\ StateToken state
  interface { value, token } = IT.fromStateValue value /\ unsafeCoerce token

foreign import data UseQuery :: Type -> Type

-- | Produces a hook for receiving and evaluating queries from a parent component.
-- |
-- | Only usable with the `componentWithQuery` function which produces the required
-- | query token; due to the request/response nature of queries they only make
-- | sense in the context of a component, not an independent hook.
-- |
-- | This should be used at most once per component. If used multiple times, only
-- | the last hook will run.
useQuery
  :: forall query slots output m
   . QueryToken query
  -> (forall a. query a -> HookM slots output m (Maybe a))
  -> Hook slots output m UseQuery Unit
useQuery token handler = Hooked $ Indexed $ liftF $ UseQuery token' handler' unit
  where
  token' :: QueryToken IT.QueryValue
  token' = unsafeCoerce token

  handler' :: forall a. IT.QueryValue a -> HookM slots output m (Maybe a)
  handler' = handler <<< IT.fromQueryValue

foreign import data UseEffect :: Type -> Type

-- | Produces a hook for running an effect on component Initialize, which can
-- | return an effect to run on component Finalize. If you would like to run
-- | your effect after every render, see `useTickEffect`.
useLifecycleEffect
  :: forall slots output m
   . HookM slots output m (Maybe (HookM slots output m Unit))
  -> Hook slots output m UseEffect Unit
useLifecycleEffect fn = Hooked $ Indexed $ liftF $ UseEffect Nothing fn unit

-- | Produces a hook for running post-render effects like subscriptions, timers,
-- | logging, and more. This replaces the usual Halogen `Initialize` and
-- | `Finalize` actions and extends them with the ability to run the same effect
-- | on each render so you don't have to manually call the effect after modifying
-- | state.
-- |
-- | The provided effect can return another effect to run as the finalizer.
-- |
-- | If the provided array of memo values is empty then the effect will run once
-- | at component mount and, if there is a provided finalizer, once at component
-- | unmount. If memo values are provided then the effect will also run on any
-- | render in which one of the memo values has changed.
-- |
-- | To run on every render:
-- |
-- | ```purs
-- | Hooks.captures {} Hooks.useTickEffect do
-- |   ...
-- | ```
-- |
-- | To run on initialize, finalize, and when a particular memo value has changed:
-- |
-- | ```purs
-- | Hooks.captures { memoA, memoB } Hooks.useEffect do
-- |   ...
-- | ```
useTickEffect
  :: forall slots output m
   . MemoValues
  -> HookM slots output m (Maybe (HookM slots output m Unit))
  -> Hook slots output m UseEffect Unit
useTickEffect memos fn = Hooked $ Indexed $ liftF $ UseEffect (Just memos) fn unit

foreign import data UseMemo :: Type -> Type -> Type

-- | When using values in let bindings within the body of a hook they will be
-- | recomputed each time the hooks body is evaluated (on render). You can use
-- | `useMemo` to prevent values from being recomputed unless a dependency has
-- | changed (as provided via `captures`).
-- |
-- | Be careful to include any arguments to the function or other dependencies
-- | which should cause the function to be re-run; if omitted you will end up
-- | with stale state.
-- |
-- | If you provide an empty set of memo values then this function will have no
-- | effect and no memoization will take place.
useMemo
  :: forall slots output m a
   . MemoValues
  -> (Unit -> a)
  -> Hook slots output m (UseMemo a) a
useMemo memos fn = Hooked $ Indexed $ liftF $ UseMemo memos to from
  where
  to :: Unit -> IT.MemoValue
  to = IT.toMemoValue <<< fn

  from :: IT.MemoValue -> a
  from = IT.fromMemoValue

foreign import data UseRef :: Type -> Type -> Type

useRef :: forall slots output m a. a -> Hook slots output m (UseRef a) (a /\ Ref a)
useRef initialValue = Hooked $ Indexed $ liftF $ UseRef initialValue' interface
  where
  initialValue' :: IT.RefValue
  initialValue' = IT.toRefValue initialValue

  interface :: { ref :: Ref IT.RefValue, value :: IT.RefValue } -> a /\ Ref a
  interface { ref, value } = IT.fromRefValue value /\ (unsafeCoerce :: Ref IT.RefValue -> Ref a) ref

-- | Use when you want to turn a stack of hooks into a new custom hook type.
publish
  :: forall hooks h' h slots output m a
   . Hooked slots output m hooks h' a
  -> Hooked slots output m hooks h a
publish = unsafeCoerce

-- | Used to improve performance for hooks which may be expensive to run on
-- | many renders (like `useTickEffect` and `useMemo`). Uses a value equality
-- | check to verify values have changed before re-running a function.
-- |
-- | Some values may be expensive to check for value equality. You can optimize
-- | this by only checking a sub-part of your captured values using `capturesWith`
captures :: forall memos a. Eq (Record memos) => Record memos -> (MemoValues -> a) -> a
captures memos fn = fn $ IT.toMemoValues $ IT.toMemoValuesImpl { eq: (==), memos }

-- | Like `captures`, but without an `Eq` constraint. Use when you only want to
-- | check part of a captured value for equality or when your captured values
-- | don't have Eq instances.
-- |
-- | This function can recreate the usual `captures`:
-- |
-- | ```purs
-- | Hooks.captures { memoA, memoB } == Hooks.capturesWith eq { memoA, memoB }
-- | ```
-- |
-- | You can also choose to improve performance by testing only a sub-part
-- | of your memoized values. Remember that this equality check is used to
-- | decide whether to re-run your effect or function, so make sure to test
-- | everything in your captures list.
-- |
-- | ```purs
-- | let
-- |   customEq memoA memoB =
-- |     memoA.user.id == memoB.user.id && memoA.data == memoB.data
-- |
-- | Hooks.capturesWith customEq { user, data }
-- | ```
capturesWith
  :: forall memos a
   . (Record memos -> Record memos -> Boolean)
  -> Record memos
  -> (MemoValues -> a)
  -> a
capturesWith memosEq memos fn =
  fn $ IT.toMemoValues $ IT.toMemoValuesImpl { eq: memosEq, memos }

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure
