module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Hooks.UseEffect (effectHook)
import Test.Hooks.UseState (stateHook)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  stateHook
  effectHook
