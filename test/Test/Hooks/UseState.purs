module Test.Hooks.UseState where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Hooks (Hook, HookM, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, mkEval, initDriver)
import Test.Setup.Log (logShouldBe, readResult)
import Test.Setup.Types (LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

type StateCount =
  { count :: Int
  , increment :: HookM Aff Unit
  }

useStateCount :: LogRef -> Hook Aff (UseState Int) StateCount
useStateCount ref = Hooks.do
  count /\ modifyCount <- Hooks.useState 0

  Hooks.pure
    { count
    , increment: modifyCount (_ + 1)
    }

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
      [ initializeSteps
      , fold $ replicate 2 [ ModifyState, RunHooks Step, Render ]
      , finalizeSteps
      ]

  where
  initializeSteps =
    [ RunHooks Initialize, Render ]

  finalizeSteps =
    [ RunHooks Finalize, Render ]
