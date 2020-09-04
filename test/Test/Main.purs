module Test.Main (main) where

import Prelude hiding (compare)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Test.Hooks.Spec as Hooks.Spec
import Test.Integration.Spec as Integration.Spec
import Test.Performance.Spec as Performance.Spec
import Test.Spec (Spec, describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (Config, defaultConfig, runSpecT)

main :: Effect Unit
main = launchAff_ $ runSpec' do
  describe "Hooks Tests" Hooks.Spec.spec
  describe "Integration Tests" Integration.Spec.spec
  describe "Performance Tests" Performance.Spec.spec

runSpec' :: Spec Unit -> Aff Unit
runSpec' = void <<< un Identity <<< runSpecT testConfig [ consoleReporter ]

testConfig :: Config
testConfig = defaultConfig { timeout = Just $ Aff.Milliseconds 30_000.0 }
