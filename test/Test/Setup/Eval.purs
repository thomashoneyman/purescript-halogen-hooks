-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Setup.Eval where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF, substFree)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenQ)
import Halogen as H
import Halogen.Aff.Driver.Eval as Aff.Driver.Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (Hook(..), HookF(..), HookM(..))
import Halogen.Hooks.Internal.Eval as Hooks.Eval
import Halogen.Hooks.Internal.Eval.Types (HookState(..), InterpretHookReason, HalogenM')
import Halogen.Hooks.Internal.Types (OutputValue, SlotType)
import Halogen.Hooks.Internal.UseHookF (UseHookF)
import Halogen.Hooks.Types (ComponentRef, StateId(..))
import Test.Setup.Log (writeLog)
import Test.Setup.Types (DriverResultState, LogRef, TestEvent(..), HalogenF')
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

evalM :: forall r q b. Ref (DriverResultState r q b) -> HalogenM' q LogRef Aff b ~> Aff
evalM ref (H.HalogenM hm) = Aff.Driver.Eval.evalM mempty ref (foldFree go hm)
  where
  go :: HalogenF' q LogRef Aff b ~> HalogenM' q LogRef Aff b
  go = case _ of
    c@(H.State f) -> do
      -- We'll report renders the same way Halogen triggers them: successful
      -- state modifications.
      DriverState { state } <- liftEffect $ Ref.read ref
      case f state of
        Tuple a state'
          | unsafeRefEq state state' ->
              -- Halogen has determined referential equality here, and so it will
              -- not trigger a re-render.
              pure unit
          | otherwise -> do
              -- Halogen has determined a state update has occurred and will now
              -- render again.
              { input } <- liftEffect $ Ref.read (unwrap state).stateRef
              writeLog Render input

      H.HalogenM $ liftF c

    c ->
      H.HalogenM $ liftF c

evalHookM :: forall q a. HalogenM' q LogRef Aff a a -> HookM Aff ~> HalogenM' q LogRef Aff a
evalHookM runHooks (HookM hm) = foldFree go hm
  where
  go :: HookF Aff ~> HalogenM' q LogRef Aff a
  go = case _ of
    c@(Modify (StateId (Tuple ref id)) f reply) -> do
      state <- H.HalogenM Hooks.Eval.getState

      case unsafeRefEq state.componentRef ref of
        true ->
          pure unit
        _ ->
          unsafeThrow "Attempted to use state-modifying HookM code outside the component where it was defined."

      let
        v = Hooks.Eval.unsafeGetCell id state.stateCells.queue

      -- Calls to `get` should not trigger evaluation. This matches with the
      -- underlying implementation of `evalHookM` and Halogen's `evalM`.
      case unsafeRefEq v (f v) of
        true ->
          pure unit
        _ ->
          writeLog ModifyState state.input

      Hooks.Eval.evalHookM runHooks (HookM $ liftF c)

    c ->
      -- For now, all other constructors are ordinary `HalogenM`
      Hooks.Eval.evalHookM runHooks (HookM $ liftF c)

interpretHook
  :: forall h q a
   . (HalogenM' q LogRef Aff a a -> HookM Aff ~> HalogenM' q LogRef Aff a)
  -> (InterpretHookReason -> HalogenM' q LogRef Aff a a)
  -> InterpretHookReason
  -> (LogRef -> Hook Aff h a)
  -> UseHookF Aff
  -- Fully expanded because type synonyms can't be partially applied
  ~> Free (H.HalogenF (HookState q LogRef Aff a) (HookM Aff Unit) SlotType OutputValue Aff)
interpretHook runHookM runHook reason hookFn = case _ of
  {-
    Left here as an example of how to insert logging into this test, but logging
    hook evaluation is too noisy at the moment. If this is needed in a special
    case, then this can be provided as an alternate interpreter to `mkEval`.

    c@(UseState initial reply) -> do
      { input: log } <- Hooks.Eval.getState
      liftAff $ writeLog (EvaluateHook UseStateHook) log
      Hooks.Eval.interpretHook runHookM runHook reason hookFn c
  -}

  c -> do
    Hooks.Eval.interpretHook runHookM runHook reason hookFn c

-- | Hooks.Eval.mkEval, specialized to local evalHookHm and interpretUseHookFn
-- | functions, and pre-specialized to `Unit` for convenience.
mkEval
  :: forall h q b
   . (LogRef -> Hook Aff h b)
  -> (Unit -> HalogenQ q (HookM Aff Unit) LogRef Unit)
  -> HalogenM' q LogRef Aff b Unit
mkEval h q = mkEvalQuery h (H.tell q)

mkEvalQuery
  :: forall h q b a
   . (LogRef -> Hook Aff h b)
  -> HalogenQ q (HookM Aff Unit) LogRef a
  -> HalogenM' q LogRef Aff b a
mkEvalQuery =
  Hooks.Eval.mkEval (\_ _ -> false) evalHookM (interpretUseHookFn evalHookM)
  where
  -- WARNING: Unlike the other functions, this one needs to be manually kept in
  -- sync with the implementation in the main Hooks library. If you change this
  -- function, also check the main library function.
  interpretUseHookFn runHookM reason hookFn = do
    { input } <- H.HalogenM Hooks.Eval.getState
    let Hook hookF = hookFn input

    writeLog (RunHooks reason) input
    a <- H.HalogenM $ substFree (interpretHook runHookM (\r -> interpretUseHookFn runHookM r hookFn) reason hookFn) hookF
    H.modify_ (over HookState _ { result = a })
    
    pure a

-- | Create a new DriverState, which can be used to evaluate multiple calls to
-- | evaluate test code, and which contains the LogRef.
-- |
-- | TODO: It should be possible to use the created driver with `evalQ` to
-- | produce a way to run actual queries; however, that would mean the driver
-- | would need to be created using the actual eval function.
-- |
-- | For more details, look at how Halogen runs components with `runUI` and
-- | returns an interface that can be used to query them. We essentially want
-- | to do that, but without the rendering.
initDriver :: forall m r q a. MonadEffect m => m (Ref (DriverResultState r q a))
initDriver = liftEffect do
  logRef <- Ref.new []

  stateRef <- Ref.new
    { input: logRef
    , componentRef: unsafeCoerce {} :: ComponentRef
    , queryFn: Nothing
    , stateCells: { queue: [], index: 0 }
    , effectCells: { queue: [], index: 0 }
    , memoCells: { queue: [], index: 0 }
    , refCells: { queue: [], index: 0 }
    , evalQueue: []
    , stateDirty: false
    }

  lifecycleHandlers <- Ref.new mempty

  map unDriverStateXRef do
    initDriverState
      { initialState: \_ -> HookState { result: unit, stateRef }
      , render: \_ -> HH.text ""
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
