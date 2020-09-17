module Halogen.Hooks.Internal.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Monad.Free (Free, liftF, substFree)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen as H
import Halogen.Hooks.Hook (Hook)
import Halogen.Hooks.HookM (HookAp(..), HookF(..), HookM(..))
import Halogen.Hooks.Internal.Eval.Types (HalogenM', HookState(..), InternalHookState, InterpretHookReason(..), fromQueryFn, toQueryFn)
import Halogen.Hooks.Internal.Types (MemoValuesImpl, OutputValue, SlotType, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.Internal.UseHookF (UseHookF(..))
import Halogen.Hooks.Types (StateId(..))
import Halogen.Query.HalogenM (HalogenAp(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

mkEval
  :: forall h q i m b a
   . (i -> i -> Boolean)
  -> (HalogenM' q i m b b -> HookM m ~> HalogenM' q i m b)
  -> (InterpretHookReason -> (i -> Hook m h b) -> HalogenM' q i m b b)
  -> (i -> Hook m h b)
  -> H.HalogenQ q (HookM m Unit) i a
  -> HalogenM' q i m b a
mkEval inputEq runHookM runHook hookFn = case _ of
  H.Initialize a -> do
    _ <- runHookAndEffects Initialize
    pure a

  H.Query q reply -> do
    { queryFn } <- H.HalogenM getState
    case queryFn of
      Nothing ->
        pure (reply unit)
      Just fn ->
        runHookM (runHookAndEffects Step)
          $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

  H.Action act a -> do
    runHookM (runHookAndEffects Step) act
    pure a

  H.Receive nextInput a -> do
    { input: prevInput } <- H.HalogenM getState

    unless (prevInput `inputEq` nextInput) do
      H.HalogenM $ modifyState_ _ { input = nextInput }
      void $ runHookAndEffects Step

    pure a

  H.Finalize a -> do
    _ <- runHookAndEffects Finalize
    pure a

  where
  runHookAndEffects reason = do
    _ <- runHook reason hookFn
    { evalQueue } <- H.HalogenM getState

    when (not (Array.null evalQueue)) do
      H.HalogenM $ modifyState_ _ { evalQueue = [], stateDirty = false }
      sequence_ evalQueue
      { stateDirty } <- H.HalogenM getState

      let initializeOrStepReason = reason == Initialize || reason == Step
      when (stateDirty && initializeOrStepReason) do
        void $ runHookAndEffects Step

    H.gets (_.result <<< unwrap)

evalHook
  :: forall hooks q i m a
   . (HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a)
  -> (InterpretHookReason -> HalogenM' q i m a a)
  -> InterpretHookReason
  -> (i -> Hook m hooks a)
  -> UseHookF m
  ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
evalHook runHookM runHook reason hookFn = case _ of
  UseState initial reply ->
    case reason of
      Initialize ->
        liftF $ H.State \state@(HookState { stateRef }) -> do
          let
            { componentRef, stateCells: { queue } } = unsafePerformEffect $ Ref.read stateRef
            newQueue = Array.snoc queue initial
            identifier = StateId (Tuple componentRef (Array.length newQueue - 1))
            _ = unsafePerformEffect $ Ref.modify_ (_ { stateCells { queue = newQueue } }) stateRef

          Tuple (reply (Tuple initial identifier)) state

      _ ->
        liftF $ H.State \state@(HookState { stateRef }) -> do
          let
            { componentRef, stateCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef
            value = unsafeGetCell index queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            identifier = StateId (Tuple componentRef index)
            _ = unsafePerformEffect $ Ref.modify_ (_ { stateCells { index = nextIndex } }) stateRef

          Tuple (reply (Tuple value identifier)) state

  UseQuery _ handler a ->
    liftF $ H.State \state@(HookState { stateRef }) -> do
      let
        handler' :: forall b. q b -> HookM m (Maybe b)
        handler' = handler <<< toQueryValue

        _ = unsafePerformEffect $ Ref.modify_ (_ { queryFn = Just $ toQueryFn handler' }) stateRef

      Tuple a state

  UseEffect mbMemos act a -> do
    case reason of
      Initialize ->
        liftF $ H.State \state@(HookState { stateRef }) -> do
          let
            eval = do
              mbFinalizer <- runHookM (runHook Queued) act

              let
                finalizer = fromMaybe (pure unit) mbFinalizer
                newQueue st = Array.snoc st.effectCells.queue (mbMemos /\ finalizer)

              H.HalogenM $ modifyState_ \st -> st { effectCells { queue = newQueue st } }

            _ = unsafePerformEffect $ Ref.modify_ (\s -> s { evalQueue = Array.snoc s.evalQueue eval }) stateRef

          Tuple a state

      Queued ->
        pure a

      Step ->
        liftF $ H.State \state@(HookState { stateRef }) ->
          let
            { effectCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            mbOldMemos /\ finalizer = unsafeGetCell index queue

          in case mbMemos, mbOldMemos of
            Just newMemos, Just oldMemos ->
              let
                memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
                memos' = { old: fromMemoValues oldMemos, new: fromMemoValues newMemos }

              in if (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) then do
                let
                  eval = do
                    mbFinalizer <- runHookM (runHook Queued) (finalizer *> act)
                    H.HalogenM $ liftF $ H.State \state' ->
                      let
                        { effectCells: { queue: queue' } } = unsafePerformEffect $ Ref.read stateRef
                        newFinalizer = fromMaybe (pure unit) mbFinalizer
                        newValue = mbMemos /\ newFinalizer
                        newQueue = unsafeSetCell index newValue queue'
                        _ = unsafePerformEffect $ Ref.modify_ (_ { effectCells { queue = newQueue } }) stateRef

                      in
                        Tuple unit state'

                  _ =
                    unsafePerformEffect $ Ref.modify_ (\st -> st
                      { evalQueue = Array.snoc st.evalQueue eval
                      , effectCells { index = nextIndex }
                      }) stateRef

                Tuple a state

              else
                let
                  _ = unsafePerformEffect $ Ref.modify_ (_ { effectCells { index = nextIndex } }) stateRef

                in
                  Tuple a state

            _, _ ->
              let
                _ = unsafePerformEffect $ Ref.modify_ (_ { effectCells { index = nextIndex } }) stateRef

              in
                Tuple a state

      Finalize ->
        liftF $ H.State \state@(HookState { stateRef }) ->
          let
            { effectCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            _ /\ finalizer = unsafeGetCell index queue
            finalizeHook = runHookM (runHook Queued) finalizer
            _ = unsafePerformEffect $ Ref.modify_
              (\s -> s
                { evalQueue = Array.snoc s.evalQueue finalizeHook
                , effectCells { index = nextIndex }
                }
              )
              stateRef

          in
            Tuple a state

  UseMemo memos memoFn reply -> do
    case reason of
      Initialize ->
        liftF $ H.State \state@(HookState { stateRef }) ->
          let
            { memoCells: { queue } } = unsafePerformEffect $ Ref.read stateRef
            newValue = memoFn unit
            _ = unsafePerformEffect $ Ref.modify_ (_ { memoCells { queue = Array.snoc queue (memos /\ newValue) } }) stateRef

          in
            Tuple (reply newValue) state

      _ ->
        liftF $ H.State \state@(HookState { stateRef }) ->
          let
            { memoCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef

            m =
              let
                oldMemos /\ oldValue = bimap fromMemoValues fromMemoValue (unsafeGetCell index queue)
                newMemos = fromMemoValues memos

              in
                { eq: newMemos.eq, old: oldMemos.memos, new: newMemos.memos, value: oldValue }

            nextIndex = if index + 1 < Array.length queue then index + 1 else 0

          in if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then
            let
              newValue = memoFn unit
              newQueue = unsafeSetCell index (memos /\ newValue) queue
              _ = unsafePerformEffect $ Ref.modify_ (_ { memoCells = { index: nextIndex, queue: newQueue } }) stateRef

            in
              Tuple (reply newValue) state

          else
            let
              _ = unsafePerformEffect $ Ref.modify_ (_ { memoCells { index = nextIndex } }) stateRef

            in
              Tuple (reply m.value) state

  UseRef initial reply ->
    case reason of
      Initialize ->
        liftF $ H.State \state@(HookState { stateRef }) ->
          let
            { refCells: { queue } } = unsafePerformEffect $ Ref.read stateRef
            ref = unsafePerformEffect $ Ref.new initial
            _ = unsafePerformEffect $ Ref.modify_ (_ { refCells { queue = Array.snoc queue ref } }) stateRef

          in
            Tuple (reply (Tuple initial ref)) state

      _ ->
        liftF $ H.State \state@(HookState { stateRef }) ->
          let
            { refCells: { index, queue } } = unsafePerformEffect $ Ref.read stateRef
            ref = unsafeGetCell index queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            value = unsafePerformEffect $ Ref.read ref
            _ = unsafePerformEffect $ Ref.modify_ (_ { refCells { index = nextIndex } }) stateRef

          in
            Tuple (reply (Tuple value ref)) state

evalHookM :: forall q i m a. HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a
evalHookM (H.HalogenM runHooks) (HookM evalUseHookF) =
  H.HalogenM $ substFree interpretHalogenHook evalUseHookF
  where
  interpretHalogenHook
    :: HookF m
    ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
  interpretHalogenHook = case _ of
    Modify (StateId (Tuple ref id)) f reply -> do
      state <- getState

      -- It is not safe to use `HookM` code which modifies state outside of the
      -- component that defines it, because the state identifiers are referring
      -- to an environment that potentially doesn't exist in the target component.
      --
      -- This leads either to unexpected state modifications or a crash when an
      -- index in state is accessed that doesn't exist.
      --
      -- NOTE: Using `unless` here throws an exception -- strictness? Using a
      -- case statement behaves as expected.
      case unsafeRefEq state.componentRef ref of
        true ->
          pure unit
        _ ->
          unsafeThrow "Attempted to use state-modifying `HookM` code outside the component where it was defined."

      let
        current = unsafeGetCell id state.stateCells.queue
        next = f current

      -- Like Halogen's implementation, `Modify` covers both get and set calls
      -- to `state`. We use the same `unsafeRefEq` technique to as Halogen does
      -- to ensure calls to `get` don't trigger evaluations / renders.
      case unsafeRefEq current next of
        true ->
          pure unit
        _ -> do
          let newQueue = unsafeSetCell id next
          modifyState_ _
            { stateCells { queue = newQueue state.stateCells.queue }
            , stateDirty = true
            }
          void runHooks

      pure (reply next)

    Subscribe eventSource reply ->
      liftF $ H.Subscribe eventSource reply

    Unsubscribe sid a ->
      liftF $ H.Unsubscribe sid a

    Lift f ->
      liftF $ H.Lift f

    ChildQuery box ->
      liftF $ H.ChildQuery box

    Raise o a ->
      liftF $ H.Raise o a

    Par (HookAp p) ->
      liftF $ H.Par $ retractFreeAp $ hoistFreeAp (HalogenAp <<< liftFreeAp <<< evalHookM (H.HalogenM runHooks)) p

    Fork hmu reply ->
      liftF $ H.Fork (evalHookM (H.HalogenM runHooks) hmu) reply

    Kill fid a ->
      liftF $ H.Kill fid a

    GetRef p reply ->
      liftF $ H.GetRef p reply

-- Read a cell for a hook
unsafeGetCell :: forall a. Int -> Array a -> a
unsafeGetCell index array = unsafePartial (Array.unsafeIndex array index)

-- Write a cell for a hook
unsafeSetCell :: forall a. Int -> a -> Array a -> Array a
unsafeSetCell index a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))

-- Read the internal Hook state without incurring a `MonadEffect` constraint
getState
  :: forall q i m a
   . Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m) (InternalHookState q i m a)
getState = do
  liftF $ H.State \state@(HookState { stateRef }) ->
    Tuple (unsafePerformEffect $ Ref.read stateRef) state

-- Modify the internal Hook state without incurring a `MonadEffect` constraint
modifyState_
  :: forall q i m a
   . (InternalHookState q i m a -> InternalHookState q i m a)
  -> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m) Unit
modifyState_ fn = do
  liftF $ H.State \state@(HookState { stateRef }) ->
    Tuple (unsafePerformEffect $ Ref.modify_ fn stateRef) state
