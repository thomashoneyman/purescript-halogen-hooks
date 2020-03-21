module Halogen.Hooks.Component
  ( component
  , componentWithQuery
  ) where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Monad.Free (foldFree, liftF)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (for_, sequence_)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks.HalogenHookM (HalogenHookAp(..), HalogenHookF(..), HalogenHookM(..), StateToken(..))
import Halogen.Hooks.Internal.Types (MemoValue, MemoValues, MemoValuesImpl, QueryToken, RefValue, StateValue, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.HookF (Hooked(..), HookF(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

component
  :: forall hooks i ps o m
   . (i -> Hooked ps o m Unit hooks (H.ComponentHTML (HalogenHookM ps o m Unit) ps m))
  -> (forall q. H.Component HH.HTML q i o m)
component hookFn = componentWithQuery (\_ i -> hookFn i)

componentWithQuery
  :: forall hooks q i ps o m
   . (QueryToken q -> i -> Hooked ps o m Unit hooks (H.ComponentHTML (HalogenHookM ps o m Unit) ps m))
  -> H.Component HH.HTML q i o m
componentWithQuery inputHookFn = do
  let
    hookFn = inputHookFn (unsafeCoerce unit :: QueryToken q)

  H.mkComponent
    { initialState
    , render: \(HookState { html }) -> html
    , eval: case _ of
        H.Initialize a -> do
          runHookFn Initialize hookFn
          pure a

        H.Query q reply -> do
          { queryFn } <- getState
          case queryFn of
            Nothing ->
              pure (reply unit)
            Just fn -> do
              let runHooks = runHookFn Step hookFn
              evalHalogenHookM runHooks $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

        H.Action act a -> do
          evalHalogenHookM (interpretHookFn Step hookFn) act
          { evalQueue } <- getState
          sequence_ evalQueue
          modifyState_ _ { evalQueue = [] }
          pure a

        H.Receive input a -> do
          modifyState_ _ { input = input }
          runHookFn Step hookFn
          pure a

        H.Finalize a -> do
          runHookFn Finalize hookFn
          pure a
    }
  where
  initialState :: i -> HookState q i ps o m
  initialState input =
    HookState
      { html: HH.text ""
      , stateRef: unsafePerformEffect $ Ref.new
          { input
          , queryFn: Nothing
          , stateCells: { queue: [], index: 0 }
          , effectCells: { queue: [], index: 0 }
          , memoCells: { queue: [], index: 0 }
          , refCells: { queue: [], index: 0 }
          , finalizerQueue: []
          , evalQueue: []
          }
      }

data InterpretHookReason
  = Initialize
  | Queued
  | Step
  | Finalize

runHookFn
  :: forall hooks q i ps o m
   . InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks (H.ComponentHTML (HalogenHookM ps o m Unit) ps m))
  -> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m Unit
runHookFn reason hookFn = do
  interpretHookFn reason hookFn
  { evalQueue } <- getState
  sequence_ evalQueue
  modifyState_ _ { evalQueue = [] }

interpretHookFn
  :: forall hooks q i ps o m
   . InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks (H.ComponentHTML (HalogenHookM ps o m Unit) ps m))
  -> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m Unit
interpretHookFn reason hookFn = do
  { input } <- getState
  let Hooked (Indexed hookF) = hookFn input
  html <- foldFree interpretHook hookF
  H.modify_ (over HookState _ { html = html })
  where
  interpretHook :: HookF ps o m ~> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m
  interpretHook = case _ of
    UseState initial reply ->
      case reason of
        Initialize -> do
          { stateCells: { queue } } <- getState

          let
            newQueue = Array.snoc queue initial
            token = StateToken (Array.length newQueue - 1)

          modifyState_ _ { stateCells { queue = newQueue } }
          pure $ reply { value: initial, token }

        _ -> do
          { stateCells: { index, queue } } <- getState

          let
            value = unsafeGetCell index queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            token = StateToken index

          modifyState_ _ { stateCells { index = nextIndex } }
          pure $ reply { value, token }

    UseQuery _ handler a ->
      case reason of
        Initialize -> do
          let
            handler' :: forall a. q a -> HalogenHookM ps o m (Maybe a)
            handler' = handler <<< toQueryValue

          modifyState_ _ { queryFn = Just (toQueryFn handler') }
          pure a

        _ ->
          pure a

    UseEffect mbMemos act a -> do
      case reason of
        Initialize -> do
          for_ mbMemos \memos -> do
            modifyState_ \st ->
              st { effectCells { queue = Array.snoc st.effectCells.queue memos } }

          let
            eval = do
              mbFinalizer <- evalHalogenHookM (interpretHookFn Queued hookFn) act
              for_ mbFinalizer \finalizer ->
                modifyState_ \st ->
                  st { finalizerQueue = Array.snoc st.finalizerQueue finalizer }

          modifyState_ \st -> st { evalQueue = Array.snoc st.evalQueue eval }

        Queued ->
          pure unit

        Step -> do
          for_ mbMemos \memos -> do
            { effectCells: { index, queue } } <- getState

            let
              newQueue = unsafeSetCell index memos queue
              nextIndex = if index + 1 < Array.length queue then index + 1 else 0

              memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
              memos' =
                { old: fromMemoValues (unsafeGetCell index queue)
                , new: fromMemoValues memos
                }

            modifyState_ _ { effectCells = { index: nextIndex, queue: newQueue } }

            when (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) do
              let eval = void $ evalHalogenHookM (interpretHookFn Queued hookFn) act
              modifyState_ \st -> st { evalQueue = Array.snoc st.evalQueue eval }

        Finalize -> do
          { finalizerQueue } <- getState
          let evalQueue = map (evalHalogenHookM mempty) finalizerQueue
          modifyState_ \st -> st { evalQueue = append st.evalQueue evalQueue }

      pure a

    UseMemo memos memoFn reply -> do
      case reason of
        Initialize -> do
          { memoCells: { queue } } <- getState

          let
            newValue = memoFn unit

          modifyState_ _ { memoCells { queue = Array.snoc queue (memos /\ newValue) } }
          pure $ reply newValue

        _ -> do
          { memoCells: { index, queue } } <- getState

          let
            m = do
              let
                oldMemos /\ oldValue = bimap fromMemoValues fromMemoValue (unsafeGetCell index queue)
                newMemos = fromMemoValues memos

              { eq: newMemos.eq
              , old: oldMemos.memos
              , new: newMemos.memos
              , value: oldValue
              }

          if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then do
            let
              nextIndex = if index + 1 < Array.length queue then index + 1 else 0
              newValue = memoFn unit
              newQueue = unsafeSetCell index (memos /\ newValue) queue

            modifyState_ _ { memoCells = { index: nextIndex, queue: newQueue } }
            pure $ reply newValue

          else
            pure $ reply m.value

    UseRef initial reply ->
      case reason of
        Initialize -> do
          { refCells: { queue } } <- getState

          let
            ref = unsafePerformEffect $ liftEffect $ Ref.new initial

          modifyState_ _ { refCells { queue = Array.snoc queue ref } }
          pure $ reply { value: initial, ref }

        _ -> do
          { refCells: { index, queue } } <- getState

          let
            ref = unsafeGetCell index queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            value = unsafePerformEffect $ liftEffect $ Ref.read ref

          modifyState_ _ { refCells { index = nextIndex } }
          pure $ reply { value, ref }

-- Interpreter

foreign import data QueryFn :: (Type -> Type) -> # Type -> Type -> (Type -> Type) -> Type

toQueryFn :: forall q ps o m. (forall a. q a -> HalogenHookM ps o m (Maybe a)) -> QueryFn q ps o m
toQueryFn = unsafeCoerce

fromQueryFn :: forall q ps o m. QueryFn q ps o m -> (forall a. q a -> HalogenHookM ps o m (Maybe a))
fromQueryFn = unsafeCoerce

newtype HookState q i ps o m = HookState
  { html :: H.ComponentHTML (HalogenHookM ps o m Unit) ps m
  , stateRef :: Ref (InternalHookState q i ps o m)
  }

derive instance newtypeHookState :: Newtype (HookState q i ps o m) _

type InternalHookState q i ps o m =
  { input :: i
  , queryFn :: Maybe (QueryFn q ps o m)
  , finalizerQueue :: Array (HalogenHookM ps o m Unit)
  , evalQueue :: Array (H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m Unit)
  , stateCells :: QueueState StateValue
  , effectCells :: QueueState MemoValues
  , memoCells :: QueueState (MemoValues /\ MemoValue)
  , refCells :: QueueState (Ref RefValue)
  }

getState
  :: forall q i ps o m
   . H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m (InternalHookState q i ps o m)
getState = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ liftEffect $ Ref.read stateRef

modifyState
  :: forall q i ps o m
   . (InternalHookState q i ps o m -> InternalHookState q i ps o m)
  -> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m (InternalHookState q i ps o m)
modifyState fn = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ liftEffect $ Ref.modify fn stateRef

modifyState_
  :: forall q i ps o m
   . (InternalHookState q i ps o m -> InternalHookState q i ps o m)
  -> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m Unit
modifyState_ fn = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ liftEffect $ Ref.modify_ fn stateRef

putState
  :: forall q i ps o m
   . InternalHookState q i ps o m
  -> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m Unit
putState s = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ liftEffect $ Ref.write s stateRef

type QueueState a =
  { queue :: Array a
  , index :: Int
  }

evalHalogenHookM
  :: forall q i ps o m
   . H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m Unit
  -> HalogenHookM ps o m
  ~> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m
evalHalogenHookM runHooks (HalogenHookM evalHookF) = foldFree interpretHalogenHook evalHookF
  where
  interpretHalogenHook :: HalogenHookF ps o m ~> H.HalogenM (HookState q i ps o m) (HalogenHookM ps o m Unit) ps o m
  interpretHalogenHook = case _ of
    Modify (StateToken token) f reply -> do
      state <- getState
      let v = f (unsafeGetCell token state.stateCells.queue)
      putState $ state { stateCells { queue = unsafeSetCell token v state.stateCells.queue } }
      runHooks
      pure (reply v)

    Subscribe eventSource reply ->
      H.HalogenM $ liftF $ H.Subscribe eventSource reply

    Unsubscribe sid a ->
      H.HalogenM $ liftF $ H.Unsubscribe sid a

    Lift f ->
      H.HalogenM $ liftF $ H.Lift f

    ChildQuery box ->
      H.HalogenM $ liftF $ H.ChildQuery box

    Raise o a ->
      H.raise o *> pure a

    Par (HalogenHookAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalHalogenHookM runHooks) p

    Fork hmu reply ->
      H.HalogenM $ liftF $ H.Fork (evalHalogenHookM runHooks hmu) reply

    Kill fid a ->
      H.HalogenM $ liftF $ H.Kill fid a

    GetRef p reply ->
      H.HalogenM $ liftF $ H.GetRef p reply

-- Utilities for updating state

unsafeGetCell :: forall a. Int -> Array a -> a
unsafeGetCell index array = unsafePartial (Array.unsafeIndex array index)

unsafeSetCell :: forall a. Int -> a -> Array a -> Array a
unsafeSetCell index a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))
