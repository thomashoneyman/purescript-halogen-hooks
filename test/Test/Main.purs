module Test.Main where

import Prelude

import Data.Array (fold)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (un)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Node.Path (resolve)
import Partial.Unsafe (unsafePartial)
import Test.Hooks.Complex.Bug5 (rerunTickAfterInitialEffectsHook)
import Test.Hooks.Primitive.UseEffect (effectHook)
import Test.Hooks.Primitive.UseMemo (memoHook)
import Test.Hooks.Primitive.UseRef (refHook)
import Test.Hooks.Primitive.UseState (stateHook)
import Test.Performance.Container (statePrefix)
import Test.Performance.Setup.Puppeteer (Browser, FilePath(..), Kilobytes, Milliseconds, Page)
import Test.Performance.Setup.Puppeteer as Puppeteer
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = launchAff_ $ withPuppeteer \browser -> runSpec' do
  describe "Primitive Tests" do
    stateHook
    effectHook
    memoHook
    refHook

  describe "Complex Tests" do
    rerunTickAfterInitialEffectsHook

  around (withMetrics browser) $ describe "Performance Tests" do

    it "??? hook" \{ runTest } -> do
      hook <- runTest (QuerySelector $ fold [ ".", statePrefix, "-hook" ])
      component <- runTest (QuerySelector $ fold [ ".", statePrefix, "-component" ])
      hook `shouldEqual` component

    it "??? hook" \{ runTest } -> do
      hook <- runTest (QuerySelector $ fold [ ".", statePrefix, "-hook" ])
      component <- runTest (QuerySelector $ fold [ ".", statePrefix, "-component" ])
      hook `shouldEqual` component

runSpec' :: Spec Unit -> Aff Unit
runSpec' = void <<< un Identity <<< runSpecT testConfig [ consoleReporter ]

testConfig :: { exit :: Boolean, slow :: Aff.Milliseconds, timeout :: Maybe Aff.Milliseconds }
testConfig = defaultConfig { timeout = Just $ Aff.Milliseconds 10000.0 }

withPuppeteer :: (Browser -> Aff Unit) -> Aff Unit
withPuppeteer = bracket Puppeteer.launch Puppeteer.closeBrowser

type PerformanceTestParameters =
  { page :: Page
  , runTest :: QuerySelector -> Aff PerformanceSummary
  }

type PerformanceSummary =
  { averageFPS :: Int
  , elapsedTime :: Milliseconds
  , heapUsed :: Kilobytes
  }

-- TODO:
--
-- Currently tests use query selectors to start tests and understand when a test
-- has completed. But it would be better to expose an interface via the window
-- object that can be used to query the Halogen application and run tests. This
-- would allow tests to:
--
--   1. Query the application and await the result; when the result is received
--      then the test is complete and the timer can stop.
--
--   2. Alternately, query the application and subscribe to output messages which
--      will record when a test has completed.
--
-- The Halogen application can register functions onto the window object at app
-- startup (in the `main` function). The `Puppeteer.evaluate` function enables
-- calling functions within Puppeteer, and the `Puppeteer.exposeFunction` function
-- enables a function which evaluates within Puppeteer to be called from outside.
--
-- Until then, though, we'll just rely on query selectors.
withMetrics :: Browser -> (PerformanceTestParameters -> Aff Unit) -> Aff Unit
withMetrics browser action = bracket initialize (_.page >>> finalize) action
  where
  initialize = do
    page <- Puppeteer.newPage browser

    path <- liftEffect $ resolve [] "test/test.html"
    Puppeteer.goto page ("file://" <> path)

    let
      runTest :: QuerySelector -> Aff PerformanceSummary
      runTest (QuerySelector selector) = do
        _ <- Puppeteer.waitForSelector page ".tests"

        Puppeteer.enableHeapProfiler page
        Puppeteer.collectGarbage page

        Puppeteer.startTrace page (FilePath "trace.json")
        initialPageMetrics <- Puppeteer.pageMetrics page

        -- Run the test to completion
        test <- Puppeteer.waitForSelector page selector
        for_ test Puppeteer.click
        _ <- Puppeteer.waitForSelector page (selector <> "-complete")

        -- Collect garbage again before collecting heap measurements; without
        -- this occasional spikes at the end of tests will throw off results.
        Puppeteer.collectGarbage page
        finalPageMetrics <- Puppeteer.pageMetrics page

        trace <- Puppeteer.stopTrace page
        mbModel <- Puppeteer.getPerformanceModel trace

        let metrics = finalPageMetrics - initialPageMetrics

        pure
          { averageFPS: Puppeteer.getAverageFPS $ unsafePartial $ fromJust mbModel
          , heapUsed: metrics.heapUsed
          , elapsedTime: metrics.timestamp
          }

    pure { runTest, page }

  finalize =
    Puppeteer.closePage
