module Test.Performance.Spec where

import Prelude hiding (compare)

import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log, logShow, warn)
import Partial.Unsafe (unsafePartial)
import Test.Setup.Performance.App as App
import Test.Setup.Performance.Measure (PerformanceSummary, TestType(..), compare, withBrowser)
import Test.Setup.Performance.Puppeteer (Milliseconds(..), Kilobytes(..))
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- Snapshots are taken by running a benchmark 10 times and averaging the results
type Snapshots =
  { v0_4_3 :: Map.Map App.Test PerformanceSummary
  }

snapshots :: Snapshots
snapshots =
  { v0_4_3: Map.fromFoldable
      [ Tuple App.StateHook
          { averageFPS: 9, elapsedTime: Milliseconds 166, heapUsed: Kilobytes 185 }
      , Tuple App.StateComponent
          { averageFPS: 19, elapsedTime: Milliseconds 101, heapUsed: Kilobytes 134 }
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
      """

    true `shouldEqual` true

  it "Should satisfy state benchmark" \browser -> do
    { hook, component } <- compare browser StateTest

    log "Hook / Snapshot"
    logShow hook
    logShow $ unsafePartial $ fromJust $ Map.lookup App.StateHook snapshots.v0_4_3

    log "Component / Snapshot"
    logShow component
    logShow $ unsafePartial $ fromJust $ Map.lookup App.StateComponent snapshots.v0_4_3

    hook.averageFPS `shouldSatisfy` (_ > component.averageFPS / 3)
    hook.elapsedTime `shouldSatisfy` (_ < component.elapsedTime * Milliseconds 2)
    hook.heapUsed `shouldSatisfy` (_ < component.heapUsed * Kilobytes 2)
