module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, pending)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.UseState (stateHook)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  stateHook
  effectHook

effectHook :: Spec Unit
effectHook = describe "useEffect" do
  pending "effect body runs on state change"
  pending "effect cleanup runs on state change"
  pending "lifecycle effect body runs only on initialize"
  pending "lifecycle effect cleanup runs only on finalize"
  pending "effect is run when memos change"
  pending "effect is skipped when memos are unchanged"
