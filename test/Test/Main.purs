module Test.Main (main) where

import Prelude hiding (compare)

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Hooks.Spec as Hooks.Spec
import Test.Integration.Spec as Integration.Spec
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Hooks Tests" Hooks.Spec.spec
  describe "Integration Tests" Integration.Spec.spec
