module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Hooks.Complex.Bug5 (rerunTickAfterInitialEffectsHook)
import Test.Hooks.Primitive.UseEffect (effectHook)
import Test.Hooks.Primitive.UseMemo (memoHook)
import Test.Hooks.Primitive.UseRef (refHook)
import Test.Hooks.Primitive.UseState (stateHook)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Primitive Tests" do
    stateHook
    effectHook
    memoHook
    refHook

  describe "Complex Tests" do
    rerunTickAfterInitialEffectsHook
