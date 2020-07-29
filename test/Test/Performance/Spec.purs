module Test.Performance.Spec where

import Prelude hiding (compare)

import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow, warn)
import Test.Performance.Test (Test(..))
import Test.Setup.Performance.Measure (PerformanceSummary, TestType(..), compare, withBrowser)
import Test.Setup.Performance.Puppeteer (Milliseconds(..), Kilobytes(..))
import Test.Setup.Performance.Puppeteer as Puppeteer
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- Snapshots are taken by running a benchmark 10 times and averaging the results
type Snapshots =
  { v0_4_3 :: Map.Map Test PerformanceSummary
  }

snapshots :: Snapshots
snapshots =
  { v0_4_3: Map.fromFoldable
      [ Tuple StateHook
          { averageFPS: 12, elapsedTime: Milliseconds 201, heapUsed: Kilobytes 266 }
      , Tuple StateComponent
          { averageFPS: 25, elapsedTime: Milliseconds 144, heapUsed: Kilobytes 171 }
      , Tuple TodoHook
          { averageFPS: 20, elapsedTime: Milliseconds 810, heapUsed: Kilobytes 2280 }
      ]
  }

-- These tests have wide acceptance ranges because of the variability of banchmarks
-- via Puppeteer in general. But they do have some light boundaries and should
-- be manually reviewed in any pull request which touches library internals.
spec :: Spec Unit
spec = around withBrowser $ describe "Performance Tests" do
  it "Should instantiate Puppeteer browser" \browser -> do
    -- We can safely disregard 'Failed to parse CPU profile' log messages. This
    -- disables those logs from this point onwards in the program execution.
    liftEffect $ Puppeteer.filterConsole

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

    log "Hook"
    logShow result.hookAverage
    log "Component"
    logShow result.componentAverage

    result.hookAverage.averageFPS `shouldSatisfy` (_ > result.componentAverage.averageFPS / 3)
    result.hookAverage.elapsedTime `shouldSatisfy` (_ < result.componentAverage.elapsedTime * Milliseconds 2)
    result.hookAverage.heapUsed `shouldSatisfy` (_ < result.componentAverage.heapUsed * Kilobytes 2)

  it "Should satisfy todo benchmark" \browser -> do
    result <- compare browser 5 TodoTest

    log "Hook"
    logShow result.hookAverage
    log "Component"
    logShow result.componentAverage
    log "Averages"

    result.hookAverage.averageFPS `shouldSatisfy` (_ > result.componentAverage.averageFPS / 3)
    result.hookAverage.elapsedTime `shouldSatisfy` (_ < result.componentAverage.elapsedTime * Milliseconds 2)
    result.hookAverage.heapUsed `shouldSatisfy` (_ < result.componentAverage.heapUsed * Kilobytes 2)
