module Test.Hooks.UseState where

import Prelude

import Data.Tuple.Nested ((/\))
import Halogen.Hooks (UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..), evalHookM, runWithQueue)
import Test.Eval (evalM, initDriver, interpretUseHookFn, readLog)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (Hook', HookM', HookType(..), LogRef, TestEvent(..))

useStateCount :: LogRef -> Hook' (UseState Int) { increment :: HookM' Unit, count :: Int }
useStateCount ref = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }

stateHook :: Spec Unit
stateHook = describe "useState" do
  it "initializes to the proper initial state value" do
    ref <- initDriver

    { count } <- evalM ref do
      runWithQueue $ interpretUseHookFn Initialize useStateCount

    log <- readLog ref

    -- The state should properly initialize
    count `shouldEqual` 0
    log `shouldEqual`
      [ RunHooks
      , EvaluateHook Initialize UseStateHook
      , Render
      ]

  it "updates state" do
    ref <- initDriver

    { count } <- evalM ref do
      { increment } <- runWithQueue $ interpretUseHookFn Initialize useStateCount

      let runHooks = runWithQueue $ interpretUseHookFn Step useStateCount

      -- increment twice
      evalHookM runHooks increment *> evalHookM runHooks increment

      runWithQueue $ interpretUseHookFn Finalize useStateCount

    log <- readLog ref

    -- The final state of the Hook should reflect the number of times it has
    -- been incremented.
    count `shouldEqual` 2
    log `shouldEqual`
      [ -- initializer
        RunHooks
      , EvaluateHook Initialize UseStateHook
      , Render

        -- state updates x2
      , ModifyState
      , RunHooks
      , EvaluateHook Step UseStateHook
      , Render

      , ModifyState
      , RunHooks
      , EvaluateHook Step UseStateHook
      , Render

        -- finalizer
      , RunHooks
      , EvaluateHook Finalize UseStateHook
      ]
