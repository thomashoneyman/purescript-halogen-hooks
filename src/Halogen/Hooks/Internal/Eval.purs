module Halogen.Hooks.Internal.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Monad.Free (foldFree, liftF)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen as H
import Halogen.Hooks.Hook (Hooked)
import Halogen.Hooks.HookM (HookAp(..), HookF(..), HookM(..), StateToken(..))
import Halogen.Hooks.Internal.Eval.Types (HookState, InternalHookState, InterpretHookReason(..), fromQueryFn, toQueryFn)
import Halogen.Hooks.Internal.Types (MemoValuesImpl, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.Internal.UseHookF (UseHookF(..))
import Partial.Unsafe (unsafePartial)

mkEval
  :: forall h q i ps o m b a
   . (H.HalogenM (HookState q i ps o m b) (HookM ps o m Unit) ps o m b -> HookM ps o m ~> H.HalogenM (HookState q i ps o m b) (HookM ps o m Unit) ps o m)
  -> (InterpretHookReason -> (i -> Hooked ps o m Unit h b) -> H.HalogenM (HookState q i ps o m b) (HookM ps o m Unit) ps o m b)
  -> (i -> Hooked ps o m Unit h b)
  -> H.HalogenQ q (HookM ps o m Unit) i a
  -> H.HalogenM (HookState q i ps o m b) (HookM ps o m Unit) ps o m a
mkEval runHookM runHook hookFn = case _ of
  H.Initialize a -> do
    -- initialize all hooks and enqueue effects
    _ <- runHookAndEffects Initialize
    -- now check all tick effects memos in case initial
    -- effects modified them
    _ <- runHookAndEffects Step
    pure a

  H.Query q reply -> do
    { queryFn } <- getState
    case queryFn of
      Nothing ->
        pure (reply unit)
      Just fn ->
        runHookM (runHookAndEffects Step)
          $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

  H.Action act a -> do
    runHookM (runHookAndEffects Step) act
    pure a

  H.Receive input a -> do
    modifyState_ _ { input = input }
    _ <- runHookAndEffects Step
    pure a

  H.Finalize a -> do
    _ <- runHookAndEffects Finalize
    pure a

  where
  runHookAndEffects reason = do
    _ <- runHook reason hookFn
    { evalQueue } <- getState
    modifyState_ _ { evalQueue = [] }
    sequence_ evalQueue
    H.gets (_.result <<< unwrap)

interpretHook
  :: forall hooks q i ps o m a
   . (H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a -> HookM ps o m ~> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m)
  -> (InterpretHookReason -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m a)
  -> InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks a)
  -> UseHookF ps o m
  ~> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m
interpretHook runHookM runHook reason hookFn = case _ of
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
            mbFinalizer <- runHookM (runHook Queued) act

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
                  -- rerun effect and get new finalizer (if any)
                  mbFinalizer <- runHookM (runHook Queued) do
                    -- run the finalizer
                    finalizer
                    -- now run the actual effect, which produces mbFinalizer
                    act

                  { effectCells: { queue: queue' } } <- getState

                  let
                    newFinalizer = fromMaybe (pure unit) mbFinalizer
                    newValue = mbMemos /\ newFinalizer
                    newQueue = unsafeSetCell index newValue queue'

                  modifyState_ _  { effectCells { queue = newQueue } }
                  -- now rerun all hooks in case the finalizer and initializer
                  -- modified state, which should trigger other
                  -- useTickEffects' effects
                  void $ runHook Step

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
          finalizeHook = runHookM (runHook Queued) finalizer

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

-- Read a cell for a hook
unsafeGetCell :: forall a. Int -> Array a -> a
unsafeGetCell index array = unsafePartial (Array.unsafeIndex array index)

-- Write a cell for a hook
unsafeSetCell :: forall a. Int -> a -> Array a -> Array a
unsafeSetCell index a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))

-- Read the internal Hook state without incurring a `MonadEffect` constraint
getState
  :: forall q i ps o m a
   . H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m (InternalHookState q i ps o m a)
getState = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.read stateRef

-- Modify the internal Hook state without incurring a `MonadEffect` constraint
modifyState
  :: forall q i ps o m a
   . (InternalHookState q i ps o m a -> InternalHookState q i ps o m a)
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m (InternalHookState q i ps o m a)
modifyState fn = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.modify fn stateRef

-- Modify the internal Hook state without incurring a `MonadEffect` constraint
modifyState_
  :: forall q i ps o m a
   . (InternalHookState q i ps o m a -> InternalHookState q i ps o m a)
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m Unit
modifyState_ fn = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.modify_ fn stateRef

-- Overwrite the internal Hook state without incurring a `MonadEffect` constraint
putState
  :: forall q i ps o m a
   . InternalHookState q i ps o m a
  -> H.HalogenM (HookState q i ps o m a) (HookM ps o m Unit) ps o m Unit
putState s = do
  { stateRef } <- H.gets unwrap
  pure $ unsafePerformEffect $ Ref.write s stateRef
