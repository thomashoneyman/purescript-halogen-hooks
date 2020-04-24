module Test.Hooks.UseState where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.Hooks (UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, mkEval, initDriver)
import Test.Setup.Log (logShouldBe, readResult)
import Test.Setup.Types (Hook', HookM', LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

useStateCount :: LogRef -> Hook' (UseState Int) { increment :: HookM' Unit, count :: Int }
useStateCount ref = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }

stateHook :: Spec Unit
stateHook = before initDriver $ describe "useState" do
  let eval = mkEval useStateCount

  it "initializes to the proper initial state value" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize
      readResult ref

    count `shouldEqual` 0

  it "updates state in response to actions" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize

      { increment } <- readResult ref
      eval (H.Action increment)
      eval (H.Action increment)

      eval H.Finalize
      readResult ref

    count `shouldEqual` 2
    logShouldBe ref $ fold
      [ [ RunHooks Initialize, Render ]
      , fold $ replicate 2 [ ModifyState, RunHooks Step, Render ]
      , [ RunHooks Finalize, Render ]
      ]
