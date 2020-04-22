-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Eval where

import Prelude

import Control.Monad.Free (foldFree, liftF)
import Control.Monad.Writer (lift, runWriterT, tell)
import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Halogen (gets)
import Halogen as H
import Halogen.Aff.Driver.Eval (evalM)
import Halogen.Aff.Driver.State (DriverState, DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (HookM(..), Hooked(..), StateToken(..), UseHookF(..))
import Halogen.Hooks as HookM
import Halogen.Hooks.Component (HookState(..), InterpretHookReason(..), initialState, unsafeGetCell, unsafeSetCell)
import Halogen.Hooks.Internal.Types (MemoValuesImpl, fromMemoValues)
import Halogen.Query.HalogenM (imapState)
import Partial.Unsafe (unsafeCrashWith)
import Test.TestM (HookState', TestF(..), TestM(..))
import Test.Types (DriverState', Hook', HookF', HookM', InternalHookState', TestEvent(..), TestWriterM, UseHookF')
import Unsafe.Coerce (unsafeCoerce)

-- Interpret `TestM` to `Aff`, given a current state. Current implementation
-- first interprets into HalogenM and then into Aff, re-using the existing
-- Halogen machinery.
evalTestM :: forall r. Ref (DriverState' r) -> TestM ~> Aff
evalTestM initRef = evalM mempty initRef <<< evalTestM'

evalTestWriterM :: forall act ps o m. TestWriterM ~> H.HalogenM HookState' act ps o m
evalTestWriterM = evalTestM' <<< map fst <<< runWriterT

evalTestM' :: forall act ps o m. TestM ~> H.HalogenM HookState' act ps o m
evalTestM' (TestM testM) = foldFree go testM
  where
  go :: TestF ~> H.HalogenM _ _ _ _ _
  go = case _ of
    State f -> do
      H.HalogenM $ liftF $ H.State f

-- Interpret `HooM` to `TestM`. See `evalHookM` matches `HookM ~> HalogenM`
evalTestHookM :: TestWriterM Unit -> HookM' ~> TestWriterM
evalTestHookM runHooks (HookM hm) = foldFree go hm
  where
  go :: HookF' ~> TestWriterM
  go = case _ of
    HookM.Modify (StateToken token) f reply -> do
      state <- getState
      let v = f (unsafeGetCell token state.stateCells.queue)
      putState $ state { stateCells { queue = unsafeSetCell token v state.stateCells.queue } }
      tell [ ModifyState ]
      runHooks
      pure (reply v)

    _ ->
      unsafeCrashWith "not implemented"

-- See `runUseHookFn`
runTestHook :: forall h. InterpretHookReason -> Hook' h ~> TestWriterM
runTestHook reason hookFn = do
  -- TODO: this `a` needs to be kept in state or something so that it doesn't
  -- get stale, and the state can be returned at the end of all this.
  a <- evalTestHook reason hookFn
  { evalQueue } <- getState
  let
    testMQueue :: Array (TestWriterM Unit)
    testMQueue = map (lift <<< interpretHalogenM <<< imapState wrap unwrap) evalQueue
  sequence_ testMQueue
  modifyState_ _ { evalQueue = [] }

  -- TODO: how to make this up to date?
  pure a

-- See `interpretUseHookFn`
evalTestHook :: forall h. InterpretHookReason -> Hook' h ~> TestWriterM
evalTestHook reason hookFn = do
  { input } <- getState
  let Hooked (Indexed hookF) = hookFn
  a <- foldFree go hookF
  case reason of
    Finalize ->
      -- don't render in the finalizer
      pure a
    _ -> do
      -- H.modify_ (over HookState _ { html = html })
      tell [ Render ]
      pure a
  where
  go :: UseHookF' ~> TestWriterM
  go h = tell [ RunHooks reason ] *> case h of
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

    UseEffect mbMemos act a -> do
      case reason of
        Initialize -> do
          let
            -- modified
            eval = imapState unwrap wrap $ evalTestWriterM do
              -- modified
              mbFinalizer <- evalTestHookM (evalTestHook Queued hookFn *> pure unit) act

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
                  -- modified
                  eval = imapState unwrap wrap $ evalTestWriterM do
                    -- modified: run finalizer
                    void $ evalTestHookM (evalTestHook Queued hookFn *> pure unit) finalizer

                    -- modified: rerun effect and get new finalizer (if any)
                    mbFinalizer <- evalTestHookM (evalTestHook Queued hookFn *> pure unit) act

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

            -- modified
            finalizeHook = imapState unwrap wrap $ evalTestWriterM do
              evalTestHookM (pure unit) finalizer

          modifyState_ \st -> st
            { evalQueue = Array.snoc st.evalQueue finalizeHook
            , effectCells { index = nextIndex }
            }

      pure a

    _ ->
      unsafeCrashWith "not implemented"

-- Create a new DriverState, which can be used to evaluate multiple calls to
-- evaluate test code.
initDriver :: forall r. Aff (Ref (DriverState' r))
initDriver = do
  lifecycleHandlers <- liftEffect $ Ref.new mempty
  map unDriverStateXRef $ liftEffect do
    initDriverState
      { initialState
      , render: \(HookState { html }) -> html
      , eval: H.mkEval H.defaultEval
      }
      unit
      mempty
      lifecycleHandlers
  where
  unDriverStateXRef
    :: forall r' s' f' act' ps' i' o'
     . Ref (DriverStateX HH.HTML r' f' o')
    -> Ref (DriverState HH.HTML r' s' f' act' ps' i' o')
  unDriverStateXRef = unsafeCoerce

getState :: TestWriterM InternalHookState'
getState = do
  { stateRef } <- gets (unwrap <<< unwrap)
  pure $ unsafePerformEffect $ Ref.read stateRef

modifyState_ :: (InternalHookState' -> InternalHookState') -> TestWriterM Unit
modifyState_ fn = do
  { stateRef } <- gets (unwrap <<< unwrap)
  pure $ unsafePerformEffect $ Ref.modify_ fn stateRef

putState :: InternalHookState' -> TestWriterM Unit
putState state = do
  { stateRef } <- H.gets (unwrap <<< unwrap)
  pure $ unsafePerformEffect $ Ref.write state stateRef

-- -- TODO: Can this be done, or does the HTML requirement ruin it?
-- evalTestHook' :: forall h. InterpretHookReason -> Hook' h ~> TestWriterM
-- evalTestHook' reason hookFn@(Hooked (Indexed hookF)) = foldFree (go reason) hookF
--   where
--   go :: UseHookF' ~> TestWriterM
--   go = interpretHalogenM <<< interpretHook reason (const hookFn)

-- TODO: If this works, can I interpret things like `evalHookM` into `TestM`,
-- and then interpret that back into `Aff` later? This would essentially mean
-- forgetting all component-specific features and focusing on only state
interpretHalogenM :: H.HalogenM HookState' (HookM' Unit) () Void Aff ~> TestM
interpretHalogenM (H.HalogenM hm) = foldFree go hm
  where
  go :: H.HalogenF HookState' (HookM' Unit) () Void Aff ~> TestM
  go = case _ of
    H.State f ->
      TestM $ liftF $ State f

    -- the remainder of these features, while important, are not relevant to
    -- Hooks implementations, which rely on existing HalogenM machinery to work.
    H.Subscribe _ _ ->
      unsafeCrashWith "subscribe not implemented"

    H.Unsubscribe _ _ ->
      unsafeCrashWith "unsubscribe not implemented"

    H.Lift _ ->
      unsafeCrashWith "lift not implemented"

    H.ChildQuery _ ->
      unsafeCrashWith "childQuery not implemented"

    H.Raise _ _ ->
      unsafeCrashWith "raise not implemented"

    H.Par _ ->
      unsafeCrashWith "par not implemented"

    H.Fork _ _ ->
      unsafeCrashWith "fork not implemented"

    H.Kill _ _ ->
      unsafeCrashWith "kill not implemented"

    H.GetRef _ _ ->
      unsafeCrashWith "getRef not implemented"
