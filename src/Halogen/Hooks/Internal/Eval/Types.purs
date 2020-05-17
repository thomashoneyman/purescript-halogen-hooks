module Halogen.Hooks.Internal.Eval.Types where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Data.Array.ST (STArray)
import Data.Array.ST as Array.ST
import Data.Array.ST.Partial as Array.ST.Partial
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Effect.Ref (Ref)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.Internal.Types (MemoValue, OutputValue, RefValue, SlotType, StateValue)
import Halogen.Hooks.Types (MemoValues, OutputToken, SlotToken)
import Partial.Unsafe (unsafePartial)
import Record.ST (STRecord)
import Record.ST as Record.ST
import Type.Row (class Cons) as Row
import Unsafe.Coerce (unsafeCoerce)

type HalogenM' q i m b a = H.HalogenM (State q i m b) (HookM m Unit) SlotType OutputValue m a

toHalogenM
  :: forall q i ps o m b a
   . SlotToken ps
  -> OutputToken o
  -> HalogenM' q i m b a
  -> H.HalogenM (State q i m b) (HookM m Unit) ps o m a
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

foreign import data QueryFn :: (Type -> Type) -> (Type -> Type) -> Type

toQueryFn :: forall q m. (forall a. q a -> HookM m (Maybe a)) -> QueryFn q m
toQueryFn = unsafeCoerce

fromQueryFn :: forall q m. QueryFn q m -> (forall a. q a -> HookM m (Maybe a))
fromQueryFn = unsafeCoerce

-- STRecord
newtype State q i m a = State
  { result :: a
  , internal :: STRecord Global (InternalState q i m a)
  }

derive instance newtypeState :: Newtype (State q i m a) _

type InternalState q i m a =
  ( input :: i
  , queryFn :: Maybe (QueryFn q m)
  , evalQueue :: STArray Global (HalogenM' q i m a Unit)
  , stateDirty :: Boolean

  , stateCells :: STArray Global StateValue
  , stateIndex :: Int

  , effectCells :: STArray Global ((Maybe MemoValues) /\ HookM m Unit)
  , effectIndex :: Int

  , memoCells :: STArray Global (MemoValues /\ MemoValue)
  , memoIndex :: Int

  , refCells :: STArray Global (Ref RefValue)
  , refIndex :: Int
  )

_input = SProxy :: SProxy "input"
_queryFn = SProxy :: SProxy "queryFn"
_evalQueue = SProxy :: SProxy "evalQueue"
_stateDirty = SProxy :: SProxy "stateDirty"
_stateCells = SProxy :: SProxy "stateCells"
_stateIndex = SProxy :: SProxy "stateIndex"
_effectCells = SProxy :: SProxy "effectCells"
_effectIndex = SProxy :: SProxy "effectIndex"
_memoCells = SProxy :: SProxy "memoCells"
_memoIndex = SProxy :: SProxy "memoIndex"
_refCells = SProxy :: SProxy "refCells"
_refIndex = SProxy :: SProxy "refIndex"

-- | Read a field from the internal state, without creating an immutable copy
getInternalField
  :: forall q i m b l a r
   . Row.Cons l a r (InternalState q i m b)
  => IsSymbol l
  => SProxy l
  -> Free (H.HalogenF (State q i m b) (HookM m Unit) SlotType OutputValue m) a
getInternalField field = do
  State { internal } <- liftF $ H.State \s -> Tuple s s
  pure $ unsafePerformEffect $ liftST $ Record.ST.peek field internal

-- | Modify a field in the internal state, without creating an immutable copy
modifyInternalField
  :: forall q i m b l a r
   . Row.Cons l a r (InternalState q i m b)
  => IsSymbol l
  => SProxy l
  -> (a -> a)
  -> Free (H.HalogenF (State q i m b) (HookM m Unit) SlotType OutputValue m) Unit
modifyInternalField field fn = do
  State { internal } <- liftF $ H.State \s -> Tuple s s
  pure $ unsafePerformEffect $ liftST $ Record.ST.modify field fn internal

pushField
  :: forall q i m b l a r
   . Row.Cons l (STArray Global a) r (InternalState q i m b)
  => IsSymbol l
  => SProxy l
  -> a
  -> Free (H.HalogenF (State q i m b) (HookM m Unit) SlotType OutputValue m) Unit
pushField l a = do
  array <- getInternalField l
  let _ = unsafePerformEffect $ liftST $ Array.ST.push a array
  pure unit

unsafeGetCell
  :: forall q i m b a
   . Int
  -> STArray Global a
  -> Free (H.HalogenF (State q i m b) (HookM m Unit) SlotType OutputValue m) a
unsafeGetCell ix arr =
  pure
    $ unsafePerformEffect
    $ liftST
    $ unsafePartial
    $ Array.ST.Partial.peek ix arr

unsafeSetCell
  :: forall q i m b a
   . Int
  -> a
  -> STArray Global a
  -> Free (H.HalogenF (State q i m b) (HookM m Unit) SlotType OutputValue m) Unit
unsafeSetCell ix a arr =
  pure
    $ unsafePerformEffect
    $ liftST
    $ unsafePartial
    $ Array.ST.Partial.poke ix a arr
