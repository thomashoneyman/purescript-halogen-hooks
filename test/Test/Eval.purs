-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Eval where

import Prelude

import Control.Monad.Free (foldFree, liftF)
import Data.Indexed (Indexed(..))
import Data.Newtype (over)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Hooks (HookF(..), HookM(..), Hooked(..), StateToken(..), UseHookF(..))
import Halogen.Hooks.Component (HookState(..), InterpretHookReason)
import Halogen.Hooks.Component as Component
import Test.Log (writeLog)
import Test.Types (DriverResultState, HalogenM', HookF', HookM', HookType(..), Hooked', LogRef, TestEvent(..), UseHookF')

-- | A convenience synonym for `Halogen.Aff.Driver.Eval.evalM`
evalM :: forall r a. Ref (DriverResultState r a) -> HalogenM' a ~> Aff
evalM initRef = Eval.evalM mempty initRef

-- | `Halogen.Hooks.HookM.evalHookM`, with an extra layer for logging.
evalHookM :: forall a. HalogenM' a a -> HookM' ~> HalogenM' a
evalHookM runHooks (HookM hm) = foldFree go hm
  where
  go :: HookF' ~> HalogenM' a
  go = case _ of
    c@(Modify (StateToken token) f reply) -> do
      { input } <- Component.getState
      liftAff $ writeLog ModifyState input -- technically could be get or modify
      Component.evalHookM runHooks (HookM $ liftF c)

    c ->
      -- For now, all other constructors are ordinary `HalogenM` and don't really
      -- need to be tested.
      Component.evalHookM runHooks (HookM $ liftF c)

-- | Replacement for `Halogen.Hooks.Component.interpretUseHookFn`.
-- |
-- | WARNING: Unlike the other functions, this one needs to be manually kept in
-- | sync with the implementation in the main Hooks library.
interpretUseHookFn :: forall h a. InterpretHookReason -> (LogRef -> Hooked' h a) -> HalogenM' a a
interpretUseHookFn reason hookFn = do
  { input: log } <- Component.getState

  liftAff $ writeLog (RunHooks reason) log
  let Hooked (Indexed hookF) = hookFn log
  a <- foldFree (interpretHook reason hookFn) hookF

  liftAff $ writeLog Render log
  H.modify_ (over HookState _ { result = a })
  pure a

-- | `Halogen.Hooks.Component.interpretHook`, with an extra layer for logging.
interpretHook :: forall h a. InterpretHookReason -> (LogRef -> Hooked' h a) -> UseHookF' ~> HalogenM' a
interpretHook reason hookFn = case _ of
  c@(UseState initial reply) -> do
    { input: log } <- Component.getState
    liftAff $ writeLog (EvaluateHook UseStateHook) log
    Component.interpretHook reason hookFn c

  c@(UseEffect _ _ _) -> do
    { input: log } <- Component.getState
    liftAff $ writeLog (EvaluateHook UseEffectHook) log
    Component.interpretHook reason hookFn c

  c -> do
    Component.interpretHook reason hookFn c
