module Performance.Main where

import Prelude hiding (compare)

import Data.Argonaut.Core (stringifyWithIndent)
import Data.Argonaut.Encode (encodeJson)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (catchException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (mkdir, writeTextFile)
import Performance.Setup.Measure (ComparisonSummary, PerformanceSummary, TestType(..), compare, testTypeToString, withBrowser)
import Performance.Snapshot (percentChange, snapshots)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ do
  runSpec' (defaultConfig { timeout = Just (Milliseconds 30_000.0) }) [ consoleReporter ] do
    describe "Peformance" spec

-- Check that every measurement is usable, not a performance regression budget.
-- Comparisons between compiler/browser versions need freshly reviewed baselines.
spec :: Spec Unit
spec = around withBrowser do
  it "Should satisfy state benchmark" \browser -> do
    liftEffect do
      catchException mempty (mkdir "test-results")

    let test = StateTest
    result <- compare browser 3 test
    for_ (result.hookResults <> result.componentResults) checkMeasurement
    liftEffect do
      writeResult test result
      Console.log "Wrote state test results to test-results (including snapshot change)."

  it "Should satisfy todo benchmark" \browser -> do
    let test = TodoTest
    result <- compare browser 3 test
    for_ (result.hookResults <> result.componentResults) checkMeasurement
    liftEffect do
      writeResult test result
      Console.log "Wrote todo test results to test-results (including snapshot change)."

checkMeasurement :: PerformanceSummary -> Aff Unit
checkMeasurement sample = do
  sample.averageFPS `shouldSatisfy` (_ > 0)
  unwrap sample.scriptTime `shouldSatisfy` (_ > 0)
  unwrap sample.totalTime `shouldSatisfy` (_ >= unwrap sample.scriptTime)
  unwrap sample.peakHeap `shouldSatisfy` (_ > 0)

writeResult :: TestType -> ComparisonSummary -> Effect Unit
writeResult test { componentAverage, hookAverage, componentResults, hookResults } = do
  writePath "summary" $ encodeJson
    { componentAverage, hookAverage }

  writePath "results" $ encodeJson
    { componentResults, hookResults }

  writePath "change" $ encodeJson $ case test of
    StateTest ->
      { componentChange:
          percentChange snapshots.state.componentAverage componentAverage
      , hookChange:
          percentChange snapshots.state.hookAverage hookAverage
      }
    TodoTest ->
      { componentChange:
          percentChange snapshots.todo.componentAverage componentAverage
      , hookChange:
          percentChange snapshots.todo.hookAverage hookAverage
      }
  where
  writePath label =
    stringifyWithIndent 2 >>> writeTextFile UTF8 (mkPath label)

  mkPath label =
    "test-results/" <> testTypeToString test <> "-" <> label <> ".json"
