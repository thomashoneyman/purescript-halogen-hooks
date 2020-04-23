module Test.Hooks.UseState where

import Prelude

import Data.Array (cons, replicate)
import Data.Foldable (fold)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Test.Eval (TestInterface(..), evalM, mkInterface)
import Test.Log (initDriver, logShouldBe)
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
    TestInterface { initialize, action, finalize } =
      mkInterface useStateCount

    hooksLog reason =
      [ RunHooks reason, EvaluateHook UseStateHook, Render ]

  it "initializes to the proper initial state value" \ref -> do
    { count } <- evalM ref initialize
    count `shouldEqual` 0

  it "updates state in response to actions" \ref -> do
    { count } <- evalM ref do
      { increment } <- initialize
      action increment *> action increment
      finalize

    count `shouldEqual` 2
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , fold $ replicate 2 $ cons ModifyState (hooksLog Step)
      , hooksLog Finalize
      ]
