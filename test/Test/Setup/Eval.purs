-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Setup.Eval where

import Prelude

import Control.Monad.Free (foldFree, liftF, substFree)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen (HalogenQ)
import Halogen as H
import Halogen.Aff.Driver.Eval as Aff.Driver.Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (Hook, HookF(..), HookM(..))
import Halogen.Hooks.Hook (unsafeFromHook)
import Halogen.Hooks.Internal.Eval as Hooks.Eval
import Halogen.Hooks.Internal.Eval.Types (HalogenM', HookState(..))
import Halogen.Hooks.Types (ComponentRef, StateId(..))
import Test.Setup.Log (writeLog)
import Test.Setup.Types (DriverResultState, LogRef, TestEvent(..), HalogenF')
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

-- Our test `evalM` function hijacks the `State` implementation from the Halogen
-- `Aff.Driver.Eval.evalM` implementation, and otherwise passes constructors
-- through. This allows us to inspect the result of a state modification and
-- log it.
--
-- WARNING: This must be kept in sync with the underlying Halogen implementation.
evalM :: forall r q b. Ref (DriverResultState r q b) -> HalogenM' q LogRef Aff b ~> Aff
evalM ref (H.HalogenM hm) = Aff.Driver.Eval.evalM mempty ref (foldFree go hm)
  where
  go :: HalogenF' q LogRef Aff b ~> HalogenM' q LogRef Aff b
  go = case _ of
    -- We'll report renders the same way Halogen triggers them: successful
    -- state modifications.
    H.State f -> H.lift do
      DriverState (st@{ state, lifecycleHandlers }) <- liftEffect (Ref.read ref)
      case f state of
        Tuple a state'
          | unsafeRefEq state state' -> do
              pure a
          | otherwise -> do
              -- First, we'll log that a render is occurring.
              { input } <- liftEffect $ Ref.read (unwrap state).stateRef
              writeLog Render input

              -- Then we'll defer to the Halogen implementation.
              liftEffect $ Ref.write (DriverState (st { state = state' })) ref
              _ <- Aff.Driver.Eval.handleLifecycle lifecycleHandlers (pure unit)
              pure a

    c ->
      H.HalogenM $ liftF c

evalHookM :: forall q a. HalogenM' q LogRef Aff a a -> HookM Aff ~> HalogenM' q LogRef Aff a
evalHookM runHooks (HookM hm) = foldFree go hm
  where
  go :: HookF Aff ~> HalogenM' q LogRef Aff a
  go = case _ of
    c@(Modify (StateId (Tuple ref id)) f _) -> do
      HookState { stateRef } <- H.get

      let state = unsafePerformEffect $ Ref.read stateRef

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

-- | Hooks.Eval.mkEval, specialized to local evalHookHm and interpretUseHookFn
-- | functions, and pre-specialized to `Unit` for convenience.
mkEval
  :: forall h q b
   . (LogRef -> Hook Aff h b)
  -> (Unit -> HalogenQ q (HookM Aff Unit) LogRef Unit)
  -> HalogenM' q LogRef Aff b Unit
mkEval h q = mkEvalQuery h (H.mkTell q)

mkEvalQuery
  :: forall h q b a
   . (LogRef -> Hook Aff h b)
  -> HalogenQ q (HookM Aff Unit) LogRef a
  -> HalogenM' q LogRef Aff b a
mkEvalQuery hookFn =
  Hooks.Eval.mkEval (\_ _ -> false) evalHookM evalHook
  where
  -- WARNING: Unlike the other functions, this one needs to be manually kept in
  -- sync with the implementation in the main Hooks library. If you change this
  -- function, also check the main library function.
  evalHook reason = do
    HookState { stateRef } <- H.get

    let
      eval = Hooks.Eval.evalHook evalHookM evalHook reason stateRef
      { input } = unsafePerformEffect $ Ref.read stateRef
      hookF = unsafeFromHook (hookFn input)

    writeLog (RunHooks reason) input
    a <- H.HalogenM (substFree eval hookF)

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
     . Ref (DriverStateX r' f' o')
    -> Ref (DriverState r' s' f' act' ps' i' o')
  unDriverStateXRef = unsafeCoerce
