module Halogen.Hooks.Component where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Monad.Free (foldFree, liftF)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (sequence_)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks.HookM (HookAp(..), HookF(..), HookM(..), StateToken(..))
import Halogen.Hooks.Internal.Types (MemoValue, MemoValues, MemoValuesImpl, QueryToken, RefValue, StateValue, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.UseHookF (Hooked(..), UseHookF(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | Produces a Halogen component from a `Hook` which returns `ComponentHTML`.
-- |
-- | Components that ought to receive continuous input from a parent component
-- | should be defined as a `Hook` with one argument, the `input` type. The
-- | resulting component will re-render every time new input is received.
-- |
-- | ```purs
-- | myComponent :: forall q i o m. H.Component q i o m
-- | myComponent = Hooks.component \input -> Hooks.do
-- |   ... hook implementation
-- | ```
component
  :: forall hooks i ps o m
   . (i -> Hooked ps o m Unit hooks (H.ComponentHTML (HookM ps o m Unit) ps m))
  -> (forall q. H.Component HH.HTML q i o m)
component hookFn = componentWithQuery (\_ i -> hookFn i)

-- | Produces a Halogen component from a `Hook` which returns `ComponentHTML`,
-- | enabling the resulting component to use queries.
-- |
-- | ```purs
-- | myComponent :: forall q i o m. H.Component q i o m
-- | myComponent = Hooks.component \input queryToken -> Hooks.do
-- |   -- the query token can be used with the `useQuery hook`
-- |   Hooks.useQuery queryToken handleQuery
-- |   ... hook implementation
-- | ```
componentWithQuery
  :: forall hooks q i ps o m
   . (QueryToken q -> i -> Hooked ps o m Unit hooks (H.ComponentHTML (HookM ps o m Unit) ps m))
  -> H.Component HH.HTML q i o m
componentWithQuery inputUseHookFn = do
  let
    hookFn = inputUseHookFn (unsafeCoerce unit :: QueryToken q)

  H.mkComponent
    { initialState
    , render: \(HookState { result }) -> result
    , eval: case _ of
        H.Initialize a -> do
          _ <- runWithQueue $ interpretUseHookFn Initialize hookFn
          pure a

        H.Query q reply -> do
          { queryFn } <- getState
          case queryFn of
            Nothing ->
              pure (reply unit)
            Just fn -> do
              let
                runHooks =
                  runWithQueue $ interpretUseHookFn Step hookFn

              evalHookM runHooks $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

        H.Action act a -> do
          evalHookM (interpretUseHookFn Step hookFn) act
          _ <- runQueue
          pure a

        H.Receive input a -> do
          modifyState_ _ { input = input }
          _ <- runWithQueue $ interpretUseHookFn Step hookFn
          pure a

        H.Finalize a -> do
          _ <- runWithQueue $ interpretUseHookFn Finalize hookFn
          pure a
    }

initialState
  :: forall q i ps o m
   . i
  -> HookState q i ps o m (H.ComponentHTML (HookM ps o m Unit) ps m)
initialState input =
  HookState
    { result: HH.text ""
    , stateRef: unsafePerformEffect $ Ref.new
        { input
        , queryFn: Nothing
        , stateCells: { queue: [], index: 0 }
        , effectCells: { queue: [], index: 0 }
        , memoCells: { queue: [], index: 0 }
        , refCells: { queue: [], index: 0 }
        , evalQueue: []
        }
    }

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

runWithQueue
  :: forall q i ps o m a
   . H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a
runWithQueue interpreter = do
  _ <- interpreter
  runQueue

runQueue
  :: forall q i ps o m a
   . H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a
runQueue = do
  { evalQueue } <- getState
  sequence_ evalQueue
  modifyState_ _ { evalQueue = [] }
  { result } <- H.gets unwrap
  pure result

interpretUseHookFn
  :: forall hooks q i ps o m a
   . InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks a)
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a
interpretUseHookFn reason hookFn = do
  { input } <- getState
  let Hooked (Indexed hookF) = hookFn input
  a <- foldFree (interpretHook reason hookFn) hookF
  H.modify_ (over HookState _ { result = a })
  { result } <- H.gets unwrap
  pure result

interpretHook
  :: forall hooks q i ps o m a
   . InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks a)
  -> UseHookF ps o m
  ~> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m
interpretHook reason hookFn = case _ of
  UseState initial reply ->
    case reason of
      Initialize -> do
        { stateCells: { queue } } <- getState

        let
          newQueue = Array.snoc queue initial
          token = StateToken (Array.length newQueue - 1)

        modifyState_ _ { stateCells { queue = newQueue } }
        pure $ reply $ Tuple initial token

      _ -> do
        { stateCells: { index, queue } } <- getState

        let
          value = unsafeGetCell index queue
          nextIndex = if index + 1 < Array.length queue then index + 1 else 0
          token = StateToken index

        modifyState_ _ { stateCells { index = nextIndex } }
        pure $ reply $ Tuple value token

  UseQuery _ handler a ->
    case reason of
      Initialize -> do
        let
          handler' :: forall res. q res -> HookM ps o m (Maybe res)
          handler' = handler <<< toQueryValue

        modifyState_ _ { queryFn = Just (toQueryFn handler') }
        pure a

      _ ->
        pure a

  UseEffect mbMemos act a -> do
    case reason of
      Initialize -> do
        let
          eval = do
            mbFinalizer <- evalHookM (interpretUseHookFn Queued hookFn) act

            let
              finalizer = fromMaybe (pure unit) mbFinalizer
              newQueue state = Array.snoc state.effectCells.queue (mbMemos /\ finalizer)

            modifyState_ \st -> st { effectCells { queue = newQueue st } }

        modifyState_ \st -> st { evalQueue = Array.snoc st.evalQueue eval }

      Queued ->
        pure unit

      Step -> do
        { effectCells: { index, queue } } <- getState

        let
          nextIndex = if index + 1 < Array.length queue then index + 1 else 0
          mbOldMemos /\ finalizer = unsafeGetCell index queue

        case mbMemos, mbOldMemos of
          Just newMemos, Just oldMemos -> do
            let
              memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
              memos' =
                { old: fromMemoValues oldMemos
                , new: fromMemoValues newMemos
                }

            if (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) then do
              let
                eval = do
                  -- run finalizer
                  void $ evalHookM (interpretUseHookFn Queued hookFn) finalizer

                  -- rerun effect and get new finalizer (if any)
                  mbFinalizer <- evalHookM (interpretUseHookFn Queued hookFn) act

                  { effectCells: { queue: queue' } } <- getState

                  let
                    newFinalizer = fromMaybe (pure unit) mbFinalizer
                    newValue = mbMemos /\ newFinalizer
                    newQueue = unsafeSetCell index newValue queue'

                  modifyState_ _  { effectCells { queue = newQueue } }

              modifyState_ \st -> st
                { evalQueue = Array.snoc st.evalQueue eval
                , effectCells { index = nextIndex }
                }

            else do
              modifyState_ _ { effectCells { index = nextIndex } }

          _, _ -> do
            -- this branch is useLifecycleEffect, so just update the index
            modifyState_ _ { effectCells { index = nextIndex } }

      Finalize -> do
        { effectCells: { index, queue } } <- getState

        let
          nextIndex = if index + 1 < Array.length queue then index + 1 else 0
          _ /\ finalizer = unsafeGetCell index queue
          finalizeHook = evalHookM (interpretUseHookFn Queued hookFn) finalizer

        modifyState_ \st -> st
          { evalQueue = Array.snoc st.evalQueue finalizeHook
          , effectCells { index = nextIndex }
          }

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

          nextIndex = if index + 1 < Array.length queue then index + 1 else 0

        if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then do
          let
            newValue = memoFn unit
            newQueue = unsafeSetCell index (memos /\ newValue) queue

          modifyState_ _ { memoCells = { index: nextIndex, queue: newQueue } }
          pure $ reply newValue

        else do
          modifyState_ _ { memoCells { index = nextIndex } }
          pure $ reply m.value

  UseRef initial reply ->
    case reason of
      Initialize -> do
        { refCells: { queue } } <- getState

        let
          ref = unsafePerformEffect $ Ref.new initial

        modifyState_ _ { refCells { queue = Array.snoc queue ref } }
        pure $ reply $ Tuple initial ref

      _ -> do
        { refCells: { index, queue } } <- getState

        let
          ref = unsafeGetCell index queue
          nextIndex = if index + 1 < Array.length queue then index + 1 else 0
          value = unsafePerformEffect $ Ref.read ref

        modifyState_ _ { refCells { index = nextIndex } }
        pure $ reply $ Tuple value ref

-- Interpreter

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

getState
  :: forall q i ps o m a
   . H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m (InternalHookState q i ps o m a)
getState = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.read stateRef

modifyState
  :: forall q i ps o m a
   . (InternalHookState q i ps o m a -> InternalHookState q i ps o m a)
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m (InternalHookState q i ps o m a)
modifyState fn = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.modify fn stateRef

modifyState_
  :: forall q i ps o m a
   . (InternalHookState q i ps o m a -> InternalHookState q i ps o m a)
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m Unit
modifyState_ fn = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.modify_ fn stateRef

putState
  :: forall q i ps o m a
   . InternalHookState q i ps o m a
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m Unit
putState s = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.write s stateRef

type QueueState a =
  { queue :: Array a
  , index :: Int
  }

evalHookM
  :: forall q i ps o m a
   . H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a
  -> HookM ps o m
  ~> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m
evalHookM runHooks (HookM evalUseHookF) = foldFree interpretHalogenHook evalUseHookF
  where
  interpretHalogenHook :: HookF ps o m ~> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m
  interpretHalogenHook = case _ of
    Modify (StateToken token) f reply -> do
      state <- getState
      let v = f (unsafeGetCell token state.stateCells.queue)
      putState $ state { stateCells { queue = unsafeSetCell token v state.stateCells.queue } }
      _ <- runHooks
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

    Par (HookAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalHookM runHooks) p

    Fork hmu reply ->
      H.HalogenM $ liftF $ H.Fork (evalHookM runHooks hmu) reply

    Kill fid a ->
      H.HalogenM $ liftF $ H.Kill fid a

    GetRef p reply ->
      H.HalogenM $ liftF $ H.GetRef p reply

-- Utilities for updating state

unsafeGetCell :: forall a. Int -> Array a -> a
unsafeGetCell index array = unsafePartial (Array.unsafeIndex array index)

unsafeSetCell :: forall a. Int -> a -> Array a -> Array a
unsafeSetCell index a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))
