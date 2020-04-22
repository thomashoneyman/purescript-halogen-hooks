module Test.Hooks.UseState where

import Prelude

import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.State (DriverState(..))
import Halogen.Hooks (UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InternalHookState, InterpretHookReason(..), runWithQueue)
import Test.Eval (LogRef, evalM, initDriver, interpretUseHookFn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (Hook', TestEvent(..), HookM')

useStateCount :: LogRef -> Hook' (UseState Int) { increment :: HookM' Unit, count :: Int }
useStateCount ref = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }

stateHook :: Spec Unit
stateHook = describe "useState" do
  it "initializes to the proper initial state value" do
    ref <- initDriver
      { increment: pure unit :: HookM' Unit
      , count: 0
      }

    _ <- evalM ref do
      runWithQueue $ interpretUseHookFn Initialize useStateCount

    -- The state should properly initialize
    -- count `shouldEqual` 0
    log :: Array TestEvent <- liftEffect do
      DriverState driver <- Ref.read ref

      let
        stateRef :: Ref (InternalHookState _ _ _ _ _ _)
        stateRef = (unwrap driver.state).stateRef

      state <- Ref.read stateRef

      let
        logRef :: Ref (Array TestEvent)
        logRef = state.input

      Ref.read logRef

    log `shouldEqual` [ RunHooks Initialize, Render ]

  -- it "updates state" do
  --   ref <- initDriver

  --   Tuple count events <- evalM ref do
  --     { increment } <- runWithQueue $ interpretUseHookFn Initialize useStateCount

  --     let runHooks = void $ runWithQueue $ interpretUseHookFn Step useStateCount

  --     -- increment twice
  --     evalHookM runHooks increment *> evalHookM runHooks increment

  --     { count } <- runWithQueue $ interpretUseHookFn Finalize useStateCount
  --     pure count

  --   -- The final state of the Hook should reflect the number of times it has
  --   -- been incremented.
  --   count `shouldEqual` 2
  --   events `shouldEqual`
  --     [ -- initializer
  --       RunHooks Initialize
  --     , Render

  --       -- state updates x2
  --     , ModifyState
  --     , RunHooks Step
  --     , Render
  --     , ModifyState
  --     , RunHooks Step
  --     , Render

  --       -- finalizer
  --     , RunHooks Finalize
  --     ]
