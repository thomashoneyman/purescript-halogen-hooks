module Test.UseState where

import Prelude

import Control.Monad.Writer (runWriterT)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (Hook, HookM, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Test.Eval (evalTestHook, evalTestHookM, evalTestM, initDriver)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (TestEvent(..))

stateHook :: Spec Unit
stateHook = describe "useState" do
  it "initializes" do
    ref <- initDriver

    Tuple { count } events <- evalTestM ref $ runWriterT do
      evalTestHook Initialize useStateCount

    count `shouldEqual` 0 *> events `shouldEqual` [ ]

  it "updates state" do
    ref <- initDriver

    Tuple increment events0 <- evalTestM ref $ runWriterT do
      { increment } <- evalTestHook Initialize useStateCount
      pure increment

    events0 `shouldEqual` [ ]

    Tuple _ events1 <- evalTestM ref $ runWriterT do
      evalTestHookM increment <* evalTestHookM increment

    events1 `shouldEqual` [ ModifyState, ModifyState ]

    Tuple count events2 <- evalTestM ref $ runWriterT do
      { count } <- evalTestHook Finalize useStateCount
      pure count

    count `shouldEqual` 2 *> events2 `shouldEqual` [ ]

useStateCount :: forall ps o m. Hook ps o m (UseState Int) { increment :: HookM ps o m Unit, count :: Int }
useStateCount = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }
