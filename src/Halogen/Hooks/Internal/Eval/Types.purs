module Halogen.Hooks.Internal.Eval.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.Internal.Types (MemoValue, OutputValue, RefValue, SlotType, StateValue)
import Halogen.Hooks.Types (MemoValues, OutputToken, SlotToken)
import Unsafe.Coerce (unsafeCoerce)

type HalogenM' q i m b a = H.HalogenM (HookState q i m b) (HookM m Unit) SlotType OutputValue m a

toHalogenM
  :: forall q i ps o m b a
   . SlotToken ps
  -> OutputToken o
  -> HalogenM' q i m b a
  -> H.HalogenM (HookState q i m b) (HookM m Unit) ps o m a
toHalogenM slotToken outputToken hm = unsafeCoerce hm

data InterpretHookReason
  = Initialize
  | Queued
  | Step
  | Finalize

derive instance eqInterpretHookReason :: Eq InterpretHookReason

instance showInterpretHookReason :: Show InterpretHookReason where
  show = case _ of
    Initialize -> "Initialize"
    Queued -> "Queued"
    Step -> "Step"
    Finalize -> "Finalize"

-- | When we run an effect, the effect may modify state
-- | If this occurs, we need to rerun the hooks via the `Step` reason,
-- | so that any `useTickEffect` memos get rechecked. If the state
-- | modified the `useTickEffect` hooks' memos, we need to finalize
-- | that hook and reinitialize it. However, this state checking
-- | should only occur when we run effects, not when hooks are being
-- | evaluated for other reasons.
-- |
-- | Ideally, we would only recheck the memos where one of the values in the
-- | memos was the state value that got changed. Unfortunately, we cannot know
-- | which state value an effect modifies when it modifies state.
-- | We can only know that a state modification occurred.
-- |
-- | NotRunningEffects = No effects are being run, so don't track any
-- |   state modifications.
-- | RunningEffects = An effect is currently running, so enable the
-- |   state modification tracking.
-- | EffectModifiedState = An effect modified state at some point.
-- |   We no longer need to track state modifications anymore because
-- |   we know we will need to recheck all `useTickEffect` memos
-- |   regardless of whether other future effects modify state, too.
data TrackStateModifications
  = NotRunningEffects
  | RunningEffects
  | EffectModifiedState

derive instance eqTrackStateModfications :: Eq TrackStateModifications

instance showTrackStateModifications :: Show TrackStateModifications where
  show = case _ of
    NotRunningEffects -> "NotRunningEffects"
    RunningEffects -> "RunningEffects"
    EffectModifiedState -> "EffectModifiedState"

foreign import data QueryFn :: (Type -> Type) -> (Type -> Type) -> Type

toQueryFn :: forall q m. (forall a. q a -> HookM m (Maybe a)) -> QueryFn q m
toQueryFn = unsafeCoerce

fromQueryFn :: forall q m. QueryFn q m -> (forall a. q a -> HookM m (Maybe a))
fromQueryFn = unsafeCoerce

newtype HookState q i m a = HookState
  { result :: a
  , stateRef :: Ref (InternalHookState q i m a)
  }

derive instance newtypeHookState :: Newtype (HookState q i m a) _

type InternalHookState q i m a =
  { input :: i
  , queryFn :: Maybe (QueryFn q m)
  , evalQueue :: Array (H.HalogenM (HookState q i m a) (HookM m Unit) SlotType OutputValue m Unit)
  , stateCells :: QueueState StateValue
  , effectCells :: QueueState ((Maybe MemoValues) /\ HookM m Unit)
  , memoCells :: QueueState (MemoValues /\ MemoValue)
  , refCells :: QueueState (Ref RefValue)
  , recheckMemos :: TrackStateModifications
  }

type QueueState a =
  { queue :: Array a
  , index :: Int
  }
