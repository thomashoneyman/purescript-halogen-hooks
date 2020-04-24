-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Setup.Eval where

import Test.Setup.Types

import Control.Monad.Free (foldFree, liftF)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff.Driver.Eval as Aff.Driver.Eval
import Halogen.Aff.Driver.State (DriverState, DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (HookF(..), HookM(..), Hooked(..), StateToken(..))
import Halogen.Hooks.Internal.Eval as Hooks.Eval
import Halogen.Hooks.Internal.Eval.Types (HookState(..), InterpretHookReason)
import Prelude (type (~>), Unit, bind, compose, discard, map, mempty, pure, unit, ($))
import Test.Setup.Log (writeLog)
import Unsafe.Coerce (unsafeCoerce)

evalM :: forall r q a. Ref (DriverResultState r q a) -> HalogenM' a ~> Aff
evalM initRef = Aff.Driver.Eval.evalM mempty initRef

evalHookM :: forall a. HalogenM' a a -> HookM' ~> HalogenM' a
evalHookM runHooks (HookM hm) = foldFree go hm
  where
  go :: HookF' ~> HalogenM' a
  go = case _ of
    c@(Modify (StateToken token) f reply) -> do
      { input } <- Hooks.Eval.getState
      writeLog ModifyState input -- technically could be get or modify
      Hooks.Eval.evalHookM runHooks (HookM $ liftF c)

    c ->
      -- For now, all other constructors are ordinary `HalogenM`
      Hooks.Eval.evalHookM runHooks (HookM $ liftF c)

interpretHook
  :: forall h a
   . (HalogenM' a a -> HookM' ~> HalogenM' a)
  -> (InterpretHookReason -> HalogenM' a a)
  -> InterpretHookReason
  -> (LogRef -> Hooked' h a)
  -> UseHookF'
  ~> HalogenM' a
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
mkEval :: forall h b. (LogRef -> Hooked' h b) -> (Unit -> HalogenQ' Unit) -> HalogenM' b Unit
mkEval h = mkEvalQuery h `compose` H.tell

mkEvalQuery :: forall h b a. (LogRef -> Hooked' h b) -> HalogenQ' a -> HalogenM' b a
mkEvalQuery = Hooks.Eval.mkEval evalHookM (interpretUseHookFn evalHookM)
  where
  -- WARNING: Unlike the other functions, this one needs to be manually kept in
  -- sync with the implementation in the main Hooks library. If you change this
  -- function, also check the main library function.
  interpretUseHookFn runHookM reason hookFn = do
    { input } <- Hooks.Eval.getState
    let Hooked (Indexed hookF) = hookFn input

    writeLog (RunHooks reason) input
    a <- foldFree (interpretHook runHookM (\r -> interpretUseHookFn runHookM r hookFn) reason hookFn) hookF

    writeLog Render input
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
    , queryFn: Nothing
    , stateCells: { queue: [], index: 0 }
    , effectCells: { queue: [], index: 0 }
    , memoCells: { queue: [], index: 0 }
    , refCells: { queue: [], index: 0 }
    , evalQueue: []
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
