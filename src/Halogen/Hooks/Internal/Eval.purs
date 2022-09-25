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
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen as H
import Halogen.Hooks.HookM (HookAp(..), HookF(..), HookM(..))
import Halogen.Hooks.Internal.Eval.Types (HookState(..), InternalHookState, InterpretHookReason(..), HalogenM', fromQueryFn, toQueryFn)
import Halogen.Hooks.Internal.Types (MemoValuesImpl, OutputValue, SlotType, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.Internal.UseHookF (UseHookF(..))
import Halogen.Hooks.Types (StateId(..))
import Halogen.Query.HalogenM (HalogenAp(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

mkEval
  :: forall q i m b a
   . (i -> i -> Boolean)
  -> (HalogenM' q i m b b -> HookM m ~> HalogenM' q i m b)
  -> (InterpretHookReason -> HalogenM' q i m b b)
  -> H.HalogenQ q (HookM m Unit) i a
  -> HalogenM' q i m b a
mkEval inputEq _evalHookM _evalHook = case _ of
  H.Initialize a -> do
    HookState { stateRef } <- H.get
    _ <- executeHooksAndEffects stateRef Initialize
    pure a

  H.Query q reply -> do
    HookState { stateRef } <- H.get
    let { queryFn } = unsafePerformEffect $ Ref.read stateRef
    case queryFn of
      Nothing ->
        pure (reply unit)
      Just fn ->
        _evalHookM (executeHooksAndEffects stateRef Step)
          $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

  H.Action act a -> do
    HookState { stateRef } <- H.get
    _evalHookM (executeHooksAndEffects stateRef Step) act
    pure a

  H.Receive nextInput a -> do
    HookState { stateRef } <- H.get
    let { input: prevInput } = unsafePerformEffect $ Ref.read stateRef

    unless (inputEq prevInput nextInput) do
      let
        execute = unsafePerformEffect do
          Ref.modify_ (_ { input = nextInput }) stateRef
          pure (executeHooksAndEffects stateRef Step)
      void execute

    pure a

  H.Finalize a -> do
    HookState { stateRef } <- H.get
    _ <- executeHooksAndEffects stateRef Finalize
    pure a

  where
  executeHooksAndEffects
    :: Ref (InternalHookState q i m b)
    -> InterpretHookReason
    -> HalogenM' q i m b b
  executeHooksAndEffects stateRef reason = do
    _ <- _evalHook reason
    let { evalQueue } = unsafePerformEffect $ Ref.read stateRef
    when (not (Array.null evalQueue)) do
      let
        runQueue = unsafePerformEffect do
          Ref.modify_ (_ { evalQueue = [], stateDirty = false }) stateRef
          pure (sequence_ evalQueue)
      runQueue
      let { stateDirty } = unsafePerformEffect $ Ref.read stateRef
      let initializeOrStepReason = reason == Initialize || reason == Step
      when (stateDirty && initializeOrStepReason) do
        void $ executeHooksAndEffects stateRef Step
    H.gets (_.result <<< unwrap)

evalHook
  :: forall q i m a
   . (HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a)
  -> (InterpretHookReason -> HalogenM' q i m a a)
  -> InterpretHookReason
  -> Ref (InternalHookState q i m a)
  -> UseHookF m ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
evalHook _evalHookM _evalHook reason stateRef = case _ of
  UseState initial reply ->
    case reason of
      Initialize -> do
        let
          identifier = unsafePerformEffect do
            { componentRef, stateCells } <- Ref.modify (\s -> s { stateCells { queue = Array.snoc s.stateCells.queue initial } }) stateRef
            pure (StateId (Tuple componentRef (Array.length stateCells.queue - 1)))
        pure (reply (Tuple initial identifier))

      _ -> do
        let
          { value, identifier } = unsafePerformEffect do
            { componentRef, stateCells: { index, queue } } <- Ref.read stateRef
            Ref.modify_ (_ { stateCells { index = stepIndex index queue } }) stateRef
            pure { value: unsafeGetCell index queue, identifier: StateId (Tuple componentRef index) }
        pure (reply (Tuple value identifier))

  UseQuery _ handler a -> do
    let
      handler' :: forall b. q b -> HookM m (Maybe b)
      handler' = handler <<< toQueryValue

    pure $ unsafePerformEffect do
      _ <- Ref.modify_ (_ { queryFn = Just $ toQueryFn handler' }) stateRef
      pure a

  UseEffect mbMemos act a ->
    case reason of
      Initialize -> pure $ unsafePerformEffect do
        let
          eval :: Int -> HalogenM' _ _ _ _ _
          eval index = do
            mbFinalizer <- _evalHookM (_evalHook Queued) act
            let finalizer = fromMaybe (pure unit) mbFinalizer
            let updateQueue st = unsafeSetCell index (mbMemos /\ finalizer) st
            pure $ unsafePerformEffect $ Ref.modify_ (\s -> s { effectCells { queue = updateQueue s.effectCells.queue } }) stateRef

          initializeState :: InternalHookState _ _ _ _ -> InternalHookState _ _ _ _
          initializeState st = st
            { evalQueue = Array.snoc st.evalQueue $ eval $ Array.length st.effectCells.queue
            , effectCells = st.effectCells { queue = Array.snoc st.effectCells.queue (mbMemos /\ pure unit) }
            }

        Ref.modify_ initializeState stateRef
        pure a

      Queued ->
        pure a

      Step -> pure $ unsafePerformEffect do
        { effectCells: { index, queue } } <- Ref.read stateRef
        let nextIndex = stepIndex index queue
        let mbOldMemos /\ finalizer = unsafeGetCell index queue

        case mbMemos, mbOldMemos of
          Just newMemos, Just oldMemos -> do
            let
              memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
              memos' = { old: fromMemoValues oldMemos, new: fromMemoValues newMemos }

            if (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) then do
              let
                eval = do
                  mbFinalizer <- _evalHookM (_evalHook Queued) (finalizer *> act)
                  let { effectCells: { queue: queue' } } = unsafePerformEffect $ Ref.read stateRef
                  let newFinalizer = fromMaybe (pure unit) mbFinalizer
                  let newValue = mbMemos /\ newFinalizer
                  let newQueue = unsafeSetCell index newValue queue'
                  pure $ unsafePerformEffect $ Ref.modify_ (_ { effectCells { queue = newQueue } }) stateRef

              Ref.modify_ (\s -> s { evalQueue = Array.snoc s.evalQueue eval, effectCells { index = nextIndex } }) stateRef
              pure a

            else do
              Ref.modify_ (_ { effectCells { index = nextIndex } }) stateRef
              pure a

          _, _ -> do
            Ref.modify_ (_ { effectCells { index = nextIndex } }) stateRef
            pure a

      Finalize -> pure $ unsafePerformEffect do
        { effectCells: { index, queue } } <- Ref.read stateRef
        let _ /\ finalizer = unsafeGetCell index queue
        let finalizeHook = _evalHookM (_evalHook Queued) finalizer
        Ref.modify_ (\s -> s { evalQueue = Array.snoc s.evalQueue finalizeHook, effectCells { index = stepIndex index queue } }) stateRef
        pure a

  UseMemo memos memoFn reply ->
    case reason of
      Initialize -> pure $ unsafePerformEffect do
        { memoCells: { queue } } <- Ref.read stateRef
        let newValue = memoFn unit
        Ref.modify_ (_ { memoCells { queue = Array.snoc queue (memos /\ newValue) } }) stateRef
        pure (reply newValue)

      _ -> pure $ unsafePerformEffect do
        { memoCells: { index, queue } } <- Ref.read stateRef

        let oldMemos /\ oldValue = bimap fromMemoValues fromMemoValue (unsafeGetCell index queue)
        let newMemos = fromMemoValues memos
        let m = { eq: newMemos.eq, old: oldMemos.memos, new: newMemos.memos, value: oldValue }
        let nextIndex = stepIndex index queue

        if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then do
          let newValue = memoFn unit
          let newQueue = unsafeSetCell index (memos /\ newValue) queue
          Ref.modify_ (_ { memoCells = { index: nextIndex, queue: newQueue } }) stateRef
          pure (reply newValue)
        else do
          Ref.modify_ (_ { memoCells { index = nextIndex } }) stateRef
          pure (reply m.value)

  UseRef initial reply ->
    case reason of
      Initialize -> pure $ unsafePerformEffect do
        { refCells: { queue } } <- Ref.read stateRef
        ref <- Ref.new initial
        Ref.modify_ (_ { refCells { queue = Array.snoc queue ref } }) stateRef
        pure (reply (Tuple initial ref))

      _ -> pure $ unsafePerformEffect do
        { refCells: { index, queue } } <- Ref.read stateRef
        let ref = unsafeGetCell index queue
        value <- Ref.read ref
        Ref.modify_ (_ { refCells { index = stepIndex index queue } }) stateRef
        pure (reply (Tuple value ref))

evalHookM :: forall q i m a. HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a
evalHookM (H.HalogenM runHooks) (HookM evalUseHookF) =
  H.HalogenM $ substFree interpretHalogenHook evalUseHookF
  where
  interpretHalogenHook :: HookF m ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
  interpretHalogenHook = case _ of
    Modify (StateId (Tuple ref id)) f reply -> do
      HookState { stateRef } <- liftF $ H.State \state -> Tuple state state

      let { componentRef, stateCells } = unsafePerformEffect $ Ref.read stateRef

      -- It is not safe to use `HookM` code which modifies state outside of the
      -- component that defines it, because the state identifiers are referring
      -- to an environment that potentially doesn't exist in the target component.
      --
      -- This leads either to unexpected state modifications or a crash when an
      -- index in state is accessed that doesn't exist.
      --
      -- NOTE: Using `unless` here throws an exception -- strictness? Using a
      -- case statement behaves as expected.
      case unsafeRefEq componentRef ref of
        true ->
          pure unit
        _ ->
          unsafeThrow "Attempted to use state-modifying `HookM` code outside the component where it was defined."

      let
        current = unsafeGetCell id stateCells.queue
        next = f current

      -- Like Halogen's implementation, `Modify` covers both get and set calls
      -- to `state`. We use the same `unsafeRefEq` technique to as Halogen does
      -- to ensure calls to `get` don't trigger evaluations / renders.
      case unsafeRefEq current next of
        true -> pure unit
        _ -> do
          let newQueue = unsafeSetCell id next
          let
            runHooks' = unsafePerformEffect do
              Ref.modify_ (\s -> s { stateCells { queue = newQueue s.stateCells.queue }, stateDirty = true }) stateRef
              pure runHooks
          void $ runHooks'

      pure (reply next)

    Subscribe emitter reply ->
      liftF $ H.Subscribe emitter reply

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

stepIndex :: forall a. Int -> Array a -> Int
stepIndex index array = if index + 1 < Array.length array then index + 1 else 0
