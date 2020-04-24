module Halogen.Hooks.Internal.Eval.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Hooks.HookM (HookM(..))
import Halogen.Hooks.Types (MemoValues)
import Halogen.Hooks.Internal.Types (StateValue, MemoValue, RefValue)
import Unsafe.Coerce (unsafeCoerce)

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

foreign import data QueryFn :: (Type -> Type) -> # Type -> Type -> (Type -> Type) -> Type

toQueryFn :: forall q ps o m. (forall a. q a -> HookM ps o m (Maybe a)) -> QueryFn q ps o m
toQueryFn = unsafeCoerce

fromQueryFn :: forall q ps o m. QueryFn q ps o m -> (forall a. q a -> HookM ps o m (Maybe a))
fromQueryFn = unsafeCoerce

newtype HookState q i ps o m a = HookState
  { result :: a
  , stateRef :: Ref (InternalHookState q i ps o m a)
  }

derive instance newtypeHookState :: Newtype (HookState q i ps o m a) _

type InternalHookState q i ps o m a =
  { input :: i
  , queryFn :: Maybe (QueryFn q ps o m)
  , evalQueue :: Array (H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m Unit)
  , stateCells :: QueueState StateValue
  , effectCells :: QueueState ((Maybe MemoValues) /\ HookM ps o m Unit)
  , memoCells :: QueueState (MemoValues /\ MemoValue)
  , refCells :: QueueState (Ref RefValue)
  }

type QueueState a =
  { queue :: Array a
  , index :: Int
  }
