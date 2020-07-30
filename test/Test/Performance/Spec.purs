module Test.Performance.Spec where

import Prelude hiding (compare)

import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow, warn)
import Test.Setup.Performance.Measure (TestType(..), compare, withBrowser)
import Test.Setup.Performance.Puppeteer (Kilobytes(..))
import Test.Setup.Performance.Puppeteer as Puppeteer
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- These tests have wide acceptance ranges because of the variability of banchmarks
-- via Puppeteer in general. But they do have some light boundaries and should
-- be manually reviewed in any pull request which touches library internals.
spec :: Spec Unit
spec = around withBrowser $ describe "Performance Tests" do
  it "Should instantiate Puppeteer browser" \browser -> do
    -- We can safely disregard 'Failed to parse CPU profile' log messages. This
    -- disables those logs from this point onwards in the program execution.
    liftEffect Puppeteer.filterConsole

    warn
      """
      You will see '-trace.json' profiles deposited to the test/ directory for
      each test in the performance benchmark. You can load any of these profiles
      into the Chrome dev tools to manually examine the performance trace (for
      example, to verify the numbers or to review the flamechart).

      Note: Chrome's processing speed can vary widely from machine to machine.
      Compare numbers from the master branch on your machine against your test
      branch on your machine, or rely on comparative numbers between the component
      and hooks implementations.
      """

    true `shouldEqual` true

  it "Should satisfy state benchmark" \browser -> do
    result <- compare browser 3 StateTest

    log "State Test: Hook (top) vs. Component (bottom)"
    logShow result.hookAverage
    logShow result.componentAverage

    result.hookAverage.averageFPS `shouldSatisfy` (_ > result.componentAverage.averageFPS / 3)
    result.hookAverage.averageHeap `shouldSatisfy` (_ < result.componentAverage.averageHeap * Kilobytes 2)

  it "Should satisfy todo benchmark" \browser -> do
    result <- compare browser 5 TodoTest

    log "Todo Test: Hook (top) vs. Component (bottom)"
    logShow result.hookAverage
    logShow result.componentAverage

    result.hookAverage.averageFPS `shouldSatisfy` (_ > result.componentAverage.averageFPS / 3)
    result.hookAverage.averageHeap `shouldSatisfy` (_ < result.componentAverage.averageHeap * Kilobytes 2)
