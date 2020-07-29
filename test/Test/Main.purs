module Test.Main where

import Prelude hiding (compare)

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Test.Hooks.Complex.Bug5 (rerunTickAfterInitialEffectsHook)
import Test.Hooks.Primitive.UseEffect (effectHook)
import Test.Hooks.Primitive.UseMemo (memoHook)
import Test.Hooks.Primitive.UseRef (refHook)
import Test.Hooks.Primitive.UseState (stateHook)
import Test.Setup.Performance.Measure (TestType(..), compare, withBrowser)
import Test.Setup.Performance.Puppeteer (Kilobytes(..), Milliseconds(..))
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main :: Effect Unit
main = launchAff_ $ runSpec' do
  describe "Primitive Tests" do
    stateHook
    effectHook
    memoHook
    refHook

  describe "Complex Tests" do
    rerunTickAfterInitialEffectsHook

  around withBrowser $ describe "Performance Tests" do

    it "Should satisfy state benchmark" \browser -> do
      { hook, component } <- compare browser StateTest

      logShow hook
      logShow component

      hook.averageFPS `shouldSatisfy` (_ < (component.averageFPS + (component.averageFPS / 5)))
      hook.elapsedTime `shouldSatisfy` (_ < (component.elapsedTime + (component.elapsedTime / Milliseconds 2)))
      hook.heapUsed `shouldSatisfy` (_ < (component.heapUsed + (component.heapUsed / Kilobytes 2)))

  where
  runSpec' :: Spec Unit -> Aff Unit
  runSpec' = void <<< un Identity <<< runSpecT testConfig [ consoleReporter ]

  testConfig :: { exit :: Boolean, slow :: Aff.Milliseconds, timeout :: Maybe Aff.Milliseconds }
  testConfig = defaultConfig { timeout = Just $ Aff.Milliseconds 10000.0 }
