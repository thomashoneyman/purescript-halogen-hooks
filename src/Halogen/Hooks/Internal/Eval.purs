module Halogen.Hooks.Internal.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Monad.Free (Free, liftF, substFree)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as Array.ST
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen as H
import Halogen.Hooks.Hook (Hooked)
import Halogen.Hooks.HookM (HookAp(..), HookF(..), HookM(..))
import Halogen.Hooks.Internal.Eval.Types (HalogenM', InterpretHookReason(..), State, fromQueryFn, toQueryFn)
import Halogen.Hooks.Internal.Eval.Types as EH
import Halogen.Hooks.Internal.Eval.Types as ET
import Halogen.Hooks.Internal.Types (MemoValuesImpl, OutputValue, SlotType, fromMemoValue, fromMemoValues, toQueryValue)
import Halogen.Hooks.Internal.UseHookF (UseHookF(..))
import Halogen.Hooks.Types (StateId(..))
import Halogen.Query.HalogenM (HalogenAp(..))
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

mkEval
  :: forall h q i m b a
   . (i -> i -> Boolean)
  -> (HalogenM' q i m b b -> HookM m ~> HalogenM' q i m b)
  -> (InterpretHookReason -> (i -> Hooked m Unit h b) -> HalogenM' q i m b b)
  -> (i -> Hooked m Unit h b)
  -> H.HalogenQ q (HookM m Unit) i a
  -> HalogenM' q i m b a
mkEval inputEq runHookM runHook hookFn = case _ of
  H.Initialize a -> do
    _ <- runHookAndEffects Initialize
    pure a

  H.Query q reply -> do
    queryFn <- H.HalogenM $ ET.getInternalField ET._queryFn
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
    prevInput <- H.HalogenM $ ET.getInternalField ET._input

    unless (prevInput `inputEq` nextInput) do
      H.HalogenM $ ET.modifyInternalField ET._input (const nextInput)
      void $ runHookAndEffects Step

    pure a

  H.Finalize a -> do
    _ <- runHookAndEffects Finalize
    pure a

  where
  runHookAndEffects reason = do
    _ <- runHook reason hookFn

    evalQueueST <- H.HalogenM $ ET.getInternalField ET._evalQueue

    -- STArray Global == Array
    let
      evalQueue = unsafeCoerce evalQueueST
      newQueue = unsafeCoerce []

    when (not Array.null evalQueue) do
      H.HalogenM $ ET.modifyInternalField ET._evalQueue (const newQueue)
      H.HalogenM $ ET.modifyInternalField ET._stateDirty (const false)
      sequence_ evalQueue
      stateDirty <- H.HalogenM $ ET.getInternalField ET._stateDirty

      let initializeOrStepReason = reason == Initialize || reason == Step
      when (stateDirty && initializeOrStepReason) do
        void $ runHookAndEffects Step

    H.gets (_.result <<< unwrap)

interpretHook
  :: forall hooks q i m a
   . (HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a)
  -> (InterpretHookReason -> HalogenM' q i m a a)
  -> InterpretHookReason
  -> (i -> Hooked m Unit hooks a)
  -> UseHookF m
  ~> Free (H.HalogenF (State q i m a) (HookM m Unit) SlotType OutputValue m)
interpretHook runHookM runHook reason hookFn = case _ of
  UseState initial reply ->
    case reason of
      Initialize -> do
        stateCells <- ET.getInternalField ET._stateCells

        let
          -- push returns the length of the modified array
          totalCells = unsafePerformEffect $ liftST $ Array.ST.push initial stateCells
          identifier = StateId (totalCells - 1)

        pure $ reply $ Tuple initial identifier

      _ -> do
        stateCells <- ET.getInternalField ET._stateCells
        stateIndex <- ET.getInternalField ET._stateIndex

        value <- ET.unsafeGetCell stateIndex stateCells

        let
          nextIndex = stepIndex stateCells stateIndex
          identifier = StateId stateIndex

        ET.modifyInternalField ET._stateIndex (const nextIndex)
        pure $ reply $ Tuple value identifier

  UseQuery _ handler a -> do
    let
      handler' :: forall b. q b -> HookM m (Maybe b)
      handler' = handler <<< toQueryValue

    ET.modifyInternalField ET._queryFn (const (Just (toQueryFn handler')))
    pure a

  UseEffect mbMemos act a -> do
    case reason of
      Initialize -> do
        let
          eval = do
            mbFinalizer <- runHookM (runHook Queued) act
            let finalizer = fromMaybe (pure unit) mbFinalizer
            H.HalogenM $ ET.pushField ET._effectCells (mbMemos /\ finalizer)

        ET.pushField ET._evalQueue eval

      Queued ->
        pure unit

      Step -> do
        effectCells <- ET.getInternalField ET._effectCells
        effectIndex <- ET.getInternalField ET._effectIndex

        mbOldMemos /\ finalizer <- ET.unsafeGetCell effectIndex effectCells

        let
          nextIndex = stepIndex effectCells effectIndex

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
                  mbFinalizer <- runHookM (runHook Queued) do
                    -- run the finalizer
                    finalizer
                    -- now run the actual effect, which produces mbFinalizer
                    act

                  let
                    newFinalizer = fromMaybe (pure unit) mbFinalizer
                    newValue = mbMemos /\ newFinalizer

                  H.HalogenM $ ET.unsafeSetCell effectIndex newValue effectCells

              ET.modifyInternalField ET._effectIndex (const nextIndex)
              ET.pushField ET._evalQueue eval

            else do
              ET.modifyInternalField ET._effectIndex (const nextIndex)

          _, _ -> do
            -- this branch is useLifecycleEffect, so just update the index
            ET.modifyInternalField ET._effectIndex (const nextIndex)

      Finalize -> do
        effectCells <- ET.getInternalField ET._effectCells
        effectIndex <- ET.getInternalField ET._effectIndex

        _ /\ finalizer <- ET.unsafeGetCell effectIndex effectCells

        let
          nextIndex = stepIndex effectCells effectIndex
          finalizeHook = runHookM (runHook Queued) finalizer

        ET.modifyInternalField ET._effectIndex (const nextIndex)
        ET.pushField ET._evalQueue finalizeHook

    pure a

  UseMemo memoValues memoFn reply -> do
    case reason of
      Initialize -> do
        let newValue = memoFn unit
        ET.pushField ET._memoCells (memoValues /\ newValue)
        pure $ reply newValue

      _ -> do
        memoCells <- ET.getInternalField ET._memoCells
        memoIndex <- ET.getInternalField ET._memoIndex

        let
          nextIndex = stepIndex memoCells memoIndex

        m <- do
          oldMemos /\ oldValue <- map (bimap fromMemoValues fromMemoValue) $ ET.unsafeGetCell memoIndex memoCells

          let
            newMemos = fromMemoValues memoValues

          pure
            { eq: newMemos.eq
            , old: oldMemos.memos
            , new: newMemos.memos
            , value: oldValue
            }

        if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then do
          let
            newValue = memoFn unit

          _ <- ET.unsafeSetCell memoIndex (memoValues /\ newValue) memoCells
          ET.modifyInternalField ET._memoIndex (const nextIndex)
          pure $ reply newValue

        else do
          ET.modifyInternalField ET._memoIndex (const nextIndex)
          pure $ reply m.value

  UseRef initial reply ->
    case reason of
      Initialize -> do
        let ref = unsafePerformEffect $ Ref.new initial
        ET.pushField ET._refCells ref
        pure $ reply $ Tuple initial ref

      _ -> do
        refCells <- ET.getInternalField ET._refCells
        refIndex <- ET.getInternalField ET._refIndex

        ref <- ET.unsafeGetCell refIndex refCells

        let
          nextIndex = stepIndex refCells refIndex
          value = unsafePerformEffect $ Ref.read ref

        EH.modifyInternalField ET._refIndex (const nextIndex)
        pure $ reply $ Tuple value ref

evalHookM :: forall q i m a. HalogenM' q i m a a -> HookM m ~> HalogenM' q i m a
evalHookM (H.HalogenM runHooks) (HookM evalUseHookF) = H.HalogenM $ substFree interpretHalogenHook evalUseHookF
  where
  interpretHalogenHook
    :: HookF m
    ~> Free (H.HalogenF (State q i m a) (HookM m Unit) SlotType OutputValue m)
  interpretHalogenHook = case _ of
    Modify (StateId token) f reply -> do
      stateCells <- ET.getInternalField ET._stateCells

      current <- ET.unsafeGetCell token stateCells

      let
        next = f current

      -- Like Halogen's implementation, `Modify` covers both get and set
      -- calls to state. We can use the same `unsafeRefEq` technique to
      -- ensure calls to `get` don't trigger evaluations.
      unless (unsafeRefEq current next) do
        ET.unsafeSetCell token next stateCells
        ET.modifyInternalField ET._stateDirty (const true)
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

stepIndex :: forall a. STArray Global a -> Int -> Int
stepIndex cells index = do
  let
    -- the runtime representation is the same
    array :: Array a
    array = unsafeCoerce cells

  if index + 1 < Array.length array then
    index + 1
  else
    0
