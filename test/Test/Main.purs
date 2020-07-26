module Test.Main where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Node.Path (resolve)
import Partial.Unsafe (unsafePartial)
import Test.Hooks.Complex.Bug5 (rerunTickAfterInitialEffectsHook)
import Test.Hooks.Primitive.UseEffect (effectHook)
import Test.Hooks.Primitive.UseMemo (memoHook)
import Test.Hooks.Primitive.UseRef (refHook)
import Test.Hooks.Primitive.UseState (stateHook)
import Test.Performance.Container as Container
import Test.Performance.Setup.Puppeteer (Browser, FilePath(..), Kilobytes, Milliseconds, Page, PageMetrics)
import Test.Performance.Setup.Puppeteer as Puppeteer
import Test.Spec (around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ withPuppeteer \browser -> runSpec [ consoleReporter ] do

  describe "Primitive Tests" do
    stateHook
    effectHook
    memoHook
    refHook

  describe "Complex Tests" do
    rerunTickAfterInitialEffectsHook

  around (withMetrics browser) $ describe "Puppeteer Tests" do
    it "runs" \{ page, query, startMetrics } -> do
      _ <- query "RunHook"
      result <- summarize page startMetrics
      traceM result
      0 `shouldEqual` 0

type PerformanceSummary =
  { averageFPS :: Int
  , elapsedTime :: Milliseconds
  , heapUsed :: Kilobytes
  }

withPuppeteer :: (Browser -> Aff Unit) -> Aff Unit
withPuppeteer = bracket Puppeteer.launch Puppeteer.closeBrowser

type PerformanceTestParameters =
  { page :: Page
  , startMetrics :: PageMetrics
  , query :: String -> Aff (Maybe Unit)
  }

withMetrics :: Browser -> (PerformanceTestParameters -> Aff Unit) -> Aff Unit
withMetrics browser action = bracket initialize (_.page >>> finalize) action
  where
  initialize = do
    page <- Puppeteer.newPage browser
    Puppeteer.debug page

    path <- liftEffect $ resolve [] "test/test.html"
    Puppeteer.goto page ("file://" <> path)

    -- TODO: Basically, I have to do all computation within Puppeteer, and only
    -- report back serializable data. For example, when the Halogen app initializes
    -- it can set a function 'query' on the window object. I can call that function
    -- via puppeteer and collect the response, but the response has to be able to
    -- come back over the wire as something primitive (I think).
    --
    -- So for example I could query, which produces a promise(?) and when it resolves
    -- as say `Unit` I can consider the test to be complete
    mbUnit <- Puppeteer.evaluate page "RunHook"
    traceM mbUnit

    let query = Puppeteer.evaluate page

    Puppeteer.enableHeapProfiler page
    Puppeteer.collectGarbage page

    Puppeteer.startTrace page (FilePath "trace.json")
    startMetrics <- Puppeteer.pageMetrics page

    pure { page, query, startMetrics }

  finalize = Puppeteer.closePage

summarize :: Page -> PageMetrics -> Aff PerformanceSummary
summarize page startMetrics = do
  -- required for accurate heap measurements, otherwise occasional spikes will
  -- throw things completely off
  Puppeteer.collectGarbage page

  endMetrics <- Puppeteer.pageMetrics page
  trace <- Puppeteer.stopTrace page
  mbModel <- Puppeteer.getPerformanceModel trace

  let metrics = startMetrics - endMetrics

  pure
    { averageFPS: Puppeteer.getAverageFPS $ unsafePartial $ fromJust mbModel
    , heapUsed: metrics.heapUsed
    , elapsedTime: metrics.timestamp
    }
