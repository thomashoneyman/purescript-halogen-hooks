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
import Halogen.Hooks.Internal.Eval.Types (HalogenM', HookState(..), InterpretHookReason(..), InternalHookState, fromQueryFn, toQueryFn)
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
    let { queryFn } = get stateRef
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
    let { input: prevInput } = get stateRef

    unless (inputEq prevInput nextInput) do
      let (_ :: Unit) = modify_ stateRef _ { input = nextInput }
      void $ executeHooksAndEffects stateRef Step

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

    let
      { evalQueue } = get stateRef

    when (not (Array.null evalQueue)) do
      let (_ :: Unit) = modify_ stateRef _ { evalQueue = [], stateDirty = false }

      sequence_ evalQueue

      let
        { stateDirty } = get stateRef
        initializeOrStepReason = reason == Initialize || reason == Step

      when (stateDirty && initializeOrStepReason) do
        void $ executeHooksAndEffects stateRef Step

    H.gets (_.result <<< unwrap)

evalHook
  :: forall q i m a
   . (HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a)
  -> (InterpretHookReason -> HalogenM' q i m a a)
  -> InterpretHookReason
  -> Ref (InternalHookState q i m a)
  -> UseHookF m
  ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
evalHook _evalHookM _evalHook reason stateRef = case _ of
  UseState initial reply ->
    case reason of
      Initialize ->
        let
          { componentRef, stateCells: { queue } } = get stateRef
          newQueue = Array.snoc queue initial
          identifier = StateId (Tuple componentRef (Array.length newQueue - 1))
          (_ :: Unit) = modify_ stateRef _ { stateCells { queue = newQueue } }

        in
          pure (reply (Tuple initial identifier))

      _ ->
        let
          { componentRef, stateCells: { index, queue } } = get stateRef
          value = unsafeGetCell index queue
          identifier = StateId (Tuple componentRef index)
          (_ :: Unit) = modify_ stateRef _ { stateCells { index = stepIndex index queue } }

        in
          pure (reply (Tuple value identifier))

  UseQuery _ handler a ->
    let
      handler' :: forall b. q b -> HookM m (Maybe b)
      handler' = handler <<< toQueryValue

      (_ :: Unit) = modify_ stateRef _ { queryFn = Just $ toQueryFn handler' }

    in
      pure a

  UseEffect mbMemos act a ->
    case reason of
      Initialize ->
        let
          eval = do
            mbFinalizer <- _evalHookM (_evalHook Queued) act

            let
              finalizer = fromMaybe (pure unit) mbFinalizer
              newQueue st = Array.snoc st.effectCells.queue (mbMemos /\ finalizer)
              (_ :: Unit) = modify_ stateRef \s -> s { effectCells { queue = newQueue s } }

            pure unit

          (_ :: Unit) = modify_ stateRef \s -> s { evalQueue = Array.snoc s.evalQueue eval }

        in
          pure a

      Queued ->
        pure a

      Step ->
        let
          { effectCells: { index, queue } } = get stateRef
          nextIndex = stepIndex index queue
          mbOldMemos /\ finalizer = unsafeGetCell index queue

        in case mbMemos, mbOldMemos of
          Just newMemos, Just oldMemos ->
            let
              memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
              memos' = { old: fromMemoValues oldMemos, new: fromMemoValues newMemos }

            in if (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) then
              let
                eval = do
                  mbFinalizer <- _evalHookM (_evalHook Queued) (finalizer *> act)

                  let
                    { effectCells: { queue: queue' } } = get stateRef
                    newFinalizer = fromMaybe (pure unit) mbFinalizer
                    newValue = mbMemos /\ newFinalizer
                    newQueue = unsafeSetCell index newValue queue'
                    (_ :: Unit) = modify_ stateRef _ { effectCells { queue = newQueue } }

                  pure unit

                (_ :: Unit) =
                  modify_ stateRef \s -> s
                    { evalQueue = Array.snoc s.evalQueue eval
                    , effectCells { index = nextIndex  }
                    }

              in
                pure a

            else
              let
                (_ :: Unit) = modify_ stateRef _ { effectCells { index = nextIndex } }

              in
                pure a

          _, _ ->
            let
              (_ :: Unit) = modify_  stateRef _ { effectCells { index = nextIndex } }

            in
              pure a

      Finalize ->
        let
          { effectCells: { index, queue } } = get stateRef
          _ /\ finalizer = unsafeGetCell index queue
          finalizeHook = _evalHookM (_evalHook Queued) finalizer
          (_ :: Unit) =
            modify_ stateRef \s -> s
              { evalQueue = Array.snoc s.evalQueue finalizeHook
              , effectCells { index = stepIndex index queue }
              }

        in
          pure a

  UseMemo memos memoFn reply ->
    case reason of
      Initialize ->
        let
          { memoCells: { queue } } = get stateRef
          newValue = memoFn unit
          (_ :: Unit) = modify_ stateRef _ { memoCells { queue = Array.snoc queue (memos /\ newValue) } }

        in
          pure (reply newValue)

      _ ->
        let
          { memoCells: { index, queue } } = get stateRef

          m =
            let
              oldMemos /\ oldValue = bimap fromMemoValues fromMemoValue (unsafeGetCell index queue)
              newMemos = fromMemoValues memos

            in
              { eq: newMemos.eq, old: oldMemos.memos, new: newMemos.memos, value: oldValue }

          nextIndex = stepIndex index queue

        in if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then
          let
            newValue = memoFn unit
            newQueue = unsafeSetCell index (memos /\ newValue) queue
            (_ :: Unit) = modify_ stateRef _ { memoCells = { index: nextIndex, queue: newQueue } }

          in
            pure (reply newValue)

        else
          let
            (_ :: Unit) = modify_ stateRef _ { memoCells { index = nextIndex } }

          in
            pure (reply m.value)

  UseRef initial reply ->
    case reason of
      Initialize ->
        let
          { refCells: { queue } } = get stateRef
          ref = unsafePerformEffect $ Ref.new initial
          (_ :: Unit) = modify_ stateRef _ { refCells { queue = Array.snoc queue ref } }

        in
          pure (reply (Tuple initial ref))

      _ ->
        let
          { refCells: { index, queue } } = get stateRef
          ref = unsafeGetCell index queue
          value = unsafePerformEffect $ Ref.read ref
          _ = modify_ stateRef _ { refCells { index = stepIndex index queue } }

        in
          pure (reply (Tuple value ref))

evalHookM :: forall q i m a. HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a
evalHookM (H.HalogenM runHooks) (HookM evalUseHookF) =
  H.HalogenM $ substFree interpretHalogenHook evalUseHookF
  where
  interpretHalogenHook
    :: HookF m
    ~> Free (H.HalogenF (HookState q i m a) (HookM m Unit) SlotType OutputValue m)
  interpretHalogenHook = case _ of
    Modify (StateId (Tuple ref id)) f reply -> do
      HookState { stateRef } <- liftF $ H.State \state -> Tuple state state

      let
        { componentRef, stateCells } = get stateRef

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
        true ->
          pure unit
        _ -> do
          let
            newQueue = unsafeSetCell id next
            (_ :: Unit) =
              modify_ stateRef \s -> s
                { stateCells { queue = newQueue s.stateCells.queue }
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

stepIndex :: forall a. Int -> Array a -> Int
stepIndex index array = if index + 1 < Array.length array then index + 1 else 0

get :: forall q i m a. Ref (InternalHookState q i m a) -> InternalHookState q i m a
get = unsafePerformEffect <<< Ref.read

modify_ :: forall q i m a. Ref (InternalHookState q i m a) -> (InternalHookState q i m a -> InternalHookState q i m a) -> Unit
modify_ ref fn = unsafePerformEffect $ Ref.modify_ fn ref
