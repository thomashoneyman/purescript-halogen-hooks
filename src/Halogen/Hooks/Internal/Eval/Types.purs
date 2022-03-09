module Halogen.Hooks.Internal.Eval.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.Internal.Types (MemoValue, OutputValue, RefValue, SlotType, StateValue)
import Halogen.Hooks.Types (ComponentRef, MemoValues, OutputToken, SlotToken)
import Unsafe.Coerce (unsafeCoerce)

type HalogenM' q i m b a = H.HalogenM (HookState q i m b) (HookM m Unit) SlotType OutputValue m a

toHalogenM
  :: forall q i ps o m b a
   . SlotToken ps
  -> OutputToken o
  -> HalogenM' q i m b a
  -> H.HalogenM (HookState q i m b) (HookM m Unit) ps o m a
toHalogenM _ _ hm = unsafeCoerce hm

data InterpretHookReason
  = Initialize
  | Queued
  | Step
  | Finalize

derive instance Eq InterpretHookReason

foreign import data QueryFn :: (Type -> Type) -> (Type -> Type) -> Type

toQueryFn :: forall q m. (forall a. q a -> HookM m (Maybe a)) -> QueryFn q m
toQueryFn = unsafeCoerce

fromQueryFn :: forall q m. QueryFn q m -> (forall a. q a -> HookM m (Maybe a))
fromQueryFn = unsafeCoerce

newtype HookState q i m a = HookState
  { result :: a
  , stateRef :: Ref (InternalHookState q i m a)
  }

derive instance Newtype (HookState q i m a) _

type InternalHookState q i m a =
  { input :: i
  , componentRef :: ComponentRef
  , queryFn :: Maybe (QueryFn q m)
  , evalQueue :: Array (H.HalogenM (HookState q i m a) (HookM m Unit) SlotType OutputValue m Unit)
  , stateCells :: QueueState StateValue
  , effectCells :: QueueState ((Maybe MemoValues) /\ HookM m Unit)
  , memoCells :: QueueState (MemoValues /\ MemoValue)
  , refCells :: QueueState (Ref RefValue)
  , stateDirty :: Boolean
  }

type QueueState a =
  { queue :: Array a
  , index :: Int
  }
