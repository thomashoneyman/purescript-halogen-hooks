module Test.Main where

import Prelude

import Control.Monad.Writer (WriterT, runWriterT)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen.Hooks (Hook, HookM, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Test.Eval (HookState', TestEvent(..), evalTestHook, evalTestHookM, evalTestM, initDriver)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TestM (TestM)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  stateHook
  effectHook

stateHook :: Spec Unit
stateHook = describe "useState" do
  it "updates state from initial value" do
    ref <- initDriver

    let
      testInit :: WriterT (Array TestEvent) (TestM HookState' Aff) _
      testInit = do
        { increment } <- evalTestHook Initialize useStateCount
        pure increment

      testIncrement :: _ -> WriterT (Array TestEvent) (TestM HookState' Aff) _
      testIncrement increment = do
        _ <- evalTestHookM increment
        _ <- evalTestHookM increment
        pure unit

      testFinalize :: WriterT (Array TestEvent) (TestM HookState' Aff) _
      testFinalize = do
        { count } <- evalTestHook Finalize useStateCount
        pure count

    Tuple increment events0 <- evalTestM ref (runWriterT testInit)
    events0 `shouldEqual` [ ]

    Tuple _ events1 <- evalTestM ref (runWriterT (testIncrement increment))
    events1 `shouldEqual` [ ModifyState, ModifyState ]

    Tuple count events2 <- evalTestM ref (runWriterT testFinalize)
    count `shouldEqual` 2

useStateCount :: forall ps o m. Hook ps o m (UseState Int) { increment :: HookM ps o m Unit, count :: Int }
useStateCount = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }

effectHook :: Spec Unit
effectHook = describe "useEffect" do
  pending "effect body runs on state change"
  pending "effect cleanup runs on state change"
  pending "lifecycle effect body runs only on initialize"
  pending "lifecycle effect cleanup runs only on finalize"
  pending "effect is run when memos change"
  pending "effect is skipped when memos are unchanged"
