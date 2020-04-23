module Test.Hooks.UseState where

import Prelude

import Data.Array (cons, replicate)
import Data.Foldable (fold)
import Data.Tuple.Nested ((/\))
import Halogen (liftAff)
import Halogen as H
import Halogen.Hooks (UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Halogen.Hooks.Component as Component
import Test.Eval (evalHookM, evalM, interpretUseHookFn)
import Test.Log (initDriver, logShouldBe, readResult)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (Hook', HookM', HookType(..), LogRef, TestEvent(..))

useStateCount :: LogRef -> Hook' (UseState Int) { increment :: HookM' Unit, count :: Int }
useStateCount ref = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }

stateHook :: Spec Unit
stateHook = before initDriver $ describe "useState" do
  let
    eval = Component.mkEval evalHookM interpretUseHookFn useStateCount
    hooksLog reason = [ RunHooks reason, EvaluateHook UseStateHook, Render ]

  it "initializes to the proper initial state value" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      liftAff $ readResult ref
    count `shouldEqual` 0

  it "updates state in response to actions" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      { increment } <- liftAff $ readResult ref
      eval (H.tell $ H.Action increment) *> eval (H.tell $ H.Action increment)
      eval $ H.tell $ H.Finalize
      liftAff $ readResult ref

    count `shouldEqual` 2
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , fold $ replicate 2 $ cons ModifyState (hooksLog Step)
      , hooksLog Finalize
      ]
