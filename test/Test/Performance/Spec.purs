module Test.Performance.Spec where

import Prelude hiding (compare)

import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log, logShow, warn)
import Partial.Unsafe (unsafePartial)
import Test.Performance.Test (Test(..))
import Test.Setup.Performance.Measure (PerformanceSummary, TestType(..), compare, measure, withBrowser)
import Test.Setup.Performance.Puppeteer (Milliseconds(..), Kilobytes(..))
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
    warn
      """
      You can safely disregard 'Failed to parse CPU profile' log messages.

      You will see '-trace.json' profiles deposited to the test/ directory for
      each test in the performance benchmark. You can load any of these profiles
      into the Chrome dev tools to manually examine the performance trace (for
      example, to verify the numbers or to review the flamechart).

      Note: Chrome's processing speed can vary widely from machine to machine.
      """

    true `shouldEqual` true

  it "Should satisfy state benchmark" \browser -> do
    { hook, component } <- compare browser StateTest

    log "Hook"
    logShow hook
    log "Component"
    logShow component

    hook.averageFPS `shouldSatisfy` (_ > component.averageFPS / 3)
    hook.elapsedTime `shouldSatisfy` (_ < component.elapsedTime * Milliseconds 2)
    hook.heapUsed `shouldSatisfy` (_ < component.heapUsed * Kilobytes 2)

  it "Should satisfy todo benchmark" \browser -> do
    -- TODO: component
    hook <- measure browser TodoHook

    log "Hook"
    logShow hook
    log "Hook Snapshot"
    logShow $ unsafePartial $ fromJust $ Map.lookup TodoHook snapshots.v0_4_3
