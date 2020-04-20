module Test.UseState where

import Prelude

import Control.Monad.Writer (runWriterT)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Test.Eval (evalTestHook, evalTestHookM, evalTestM, initDriver)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (Hook', TestEvent(..), HookM')

useStateCount :: Hook' (UseState Int) { increment :: HookM' Unit, count :: Int }
useStateCount = Hooks.do
  count /\ countState <- Hooks.useState 0
  Hooks.pure { count, increment: Hooks.modify_ countState (_ + 1) }

stateHook :: Spec Unit
stateHook = describe "useState" do
  it "initializes" do
    ref <- initDriver

    Tuple { count } events <- evalTestM ref $ runWriterT do
      evalTestHook Initialize useStateCount

    -- The state should properly initialize
    count `shouldEqual` 0 *> events `shouldEqual` [ ]

  it "updates state" do
    ref <- initDriver

    Tuple count events <- evalTestM ref $ runWriterT do
      { increment } <- evalTestHook Initialize useStateCount

      -- increment twice
      evalTestHookM increment *> evalTestHookM increment

      { count } <- evalTestHook Finalize useStateCount
      pure count

    -- The final state of the Hook should reflect the number of times it has
    -- been incremented.
    count `shouldEqual` 2 *> events `shouldEqual` [ ModifyState, ModifyState ]
