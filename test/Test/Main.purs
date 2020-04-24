module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Hooks.UseEffect (effectHook)
import Test.Hooks.UseMemo (memoHook)
import Test.Hooks.UseRef (refHook)
import Test.Hooks.UseState (stateHook)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  stateHook
  effectHook
  memoHook
  refHook
