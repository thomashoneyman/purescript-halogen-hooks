module Test.Main where

import Prelude hiding (compare)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Test.Hooks.Complex.Bug5 (rerunTickAfterInitialEffectsHook)
import Test.Hooks.Primitive.UseEffect (effectHook)
import Test.Hooks.Primitive.UseMemo (memoHook)
import Test.Hooks.Primitive.UseRef (refHook)
import Test.Hooks.Primitive.UseState (stateHook)
import Test.Performance.Spec as Performance
import Test.Spec (Spec, describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (Config, defaultConfig, runSpecT)

main :: Effect Unit
main = launchAff_ $ runSpec' do
  describe "Primitive Tests" do
    stateHook
    effectHook
    memoHook
    refHook

  describe "Complex Tests" do
    rerunTickAfterInitialEffectsHook

  describe "Performance Tests" do
    Performance.spec

  where
  runSpec' :: Spec Unit -> Aff Unit
  runSpec' = void <<< un Identity <<< runSpecT testConfig [ consoleReporter ]

  testConfig :: Config
  testConfig = defaultConfig { timeout = Just $ Aff.Milliseconds 30_000.0 }
