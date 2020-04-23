-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Eval where

import Prelude

import Control.Monad.Free (foldFree, liftF)
import Data.Const (Const)
import Data.Coyoneda (unCoyoneda)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, wrap)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Hooks (HookF(..), HookM(..), Hooked(..), StateToken(..), UseHookF(..))
import Halogen.Hooks.Component (HookState(..), InterpretHookReason(..), fromQueryFn, runWithQueue)
import Halogen.Hooks.Component as Component
import Test.Log (writeLog)
import Test.Types (DriverResultState, HalogenM', Hook', HookF', HookM', HookType(..), LogRef, TestEvent(..), UseHookF')

-- | A convenience for wrapping up calls to `evalHookM` and `interpretHook`. This
-- | should be used when setting up tests:
-- |
-- | ```purs
-- | let EvalSpec { initialize, handleAction, finalize } = mkEvalSpec myHookFn
-- |
-- | ...
-- | result <- evalM ref do
-- |   { myHookAction } <- initialize
-- |   handleAction myHookAction
-- |   finalize
-- | ```
newtype Eval b a = Eval (H.HalogenQ (Const Void) (HookM' Unit) LogRef a -> HalogenM' b a)

derive instance newtypeTestInterface :: Newtype (Eval b a) _

-- | TODO: By passing in `interpretUseHookFn` this can use the exact implementation
-- | used by `mkComponent`, sharing as much code as possible. At that point only
-- | the definition of `interpretUseHookFn` is any different.
mkEval :: forall h b a. (LogRef -> Hook' h b) -> Eval b a
mkEval hookFn = wrap case _ of
  H.Initialize a -> do
    _ <- runWithQueue $ interpretUseHookFn Initialize hookFn
    pure a

  H.Query q reply -> do
    { queryFn } <- Component.getState
    case queryFn of
      Nothing ->
        pure (reply unit)
      Just fn -> do
        let
          runHooks =
            runWithQueue $ interpretUseHookFn Step hookFn

        evalHookM runHooks $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) q

  H.Action act a -> do
    evalHookM (runWithQueue $ interpretUseHookFn Step hookFn) act
    pure a

  H.Receive input a -> do
    Component.modifyState_ _ { input = input }
    _ <- runWithQueue $ interpretUseHookFn Step hookFn
    pure a

  H.Finalize a -> do
    _ <- runWithQueue $ interpretUseHookFn Finalize hookFn
    pure a

-- | `Halogen.Aff.Driver.Eval.evalM`, with an extra layer for logging.
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
interpretUseHookFn :: forall h a. InterpretHookReason -> (LogRef -> Hook' h a) -> HalogenM' a a
interpretUseHookFn reason hookFn = do
  { input: log } <- Component.getState

  liftAff $ writeLog (RunHooks reason) log
  let Hooked (Indexed hookF) = hookFn log
  a <- foldFree (interpretHook reason hookFn) hookF

  liftAff $ writeLog Render log
  H.modify_ (over HookState _ { result = a })
  pure a

-- | `Halogen.Hooks.Component.interpretHook`, with an extra layer for logging.
interpretHook :: forall h a. InterpretHookReason -> (LogRef -> Hook' h a) -> UseHookF' ~> HalogenM' a
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
