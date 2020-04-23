-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Setup.Eval where

import Test.Setup.Types

import Control.Monad.Free (foldFree, liftF)
import Data.Indexed (Indexed(..))
import Data.Newtype (over)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff.Driver.Eval as Aff.Driver.Eval
import Halogen.Hooks (HookF(..), HookM(..), Hooked(..), StateToken(..))
import Halogen.Hooks.Internal.Eval as Hooks.Eval
import Halogen.Hooks.Internal.Eval.Types (HookState(..), InterpretHookReason)
import Prelude (type (~>), bind, discard, mempty, pure, ($))
import Test.Setup.Log (writeLog)

evalM :: forall r a. Ref (DriverResultState r a) -> HalogenM' a ~> Aff
evalM initRef = Aff.Driver.Eval.evalM mempty initRef

evalHookM :: forall a. HalogenM' a a -> HookM' ~> HalogenM' a
evalHookM runHooks (HookM hm) = foldFree go hm
  where
  go :: HookF' ~> HalogenM' a
  go = case _ of
    c@(Modify (StateToken token) f reply) -> do
      { input } <- Hooks.Eval.getState
      liftAff $ writeLog ModifyState input -- technically could be get or modify
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

mkEval :: forall h b a. (LogRef -> Hooked' h b) -> HalogenQ' a -> HalogenM' b a
mkEval = Hooks.Eval.mkEval evalHookM (interpretUseHookFn evalHookM)
  where
  -- WARNING: Unlike the other functions, this one needs to be manually kept in
  -- sync with the implementation in the main Hooks library. If you change this
  -- function, also check the main library function.
  interpretUseHookFn runHookM reason hookFn = do
    { input } <- Hooks.Eval.getState
    let Hooked (Indexed hookF) = hookFn input

    liftAff $ writeLog (RunHooks reason) input
    a <- foldFree (interpretHook runHookM (\r -> interpretUseHookFn runHookM r hookFn) reason hookFn) hookF

    liftAff $ writeLog Render input
    H.modify_ (over HookState _ { result = a })
    pure a
