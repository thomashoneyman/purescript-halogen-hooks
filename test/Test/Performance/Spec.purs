module Test.Performance.Spec (spec) where

import Prelude hiding (compare)

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (mkdir, writeTextFile)
import Test.Setup.Performance.Measure (ComparisonSummary, TestType(..), compare, testTypeToString, withBrowser)
import Test.Setup.Performance.Puppeteer (Kilobytes(..))
import Test.Setup.Performance.Puppeteer as Puppeteer
import Test.Spec (Spec, around, it)
import Test.Spec.Assertions (shouldSatisfy)

-- These tests have wide acceptance ranges because of the variability of banchmarks
-- via Puppeteer in general. But they do have some light boundaries and should
-- be manually reviewed in any pull request which touches library internals.
spec :: Spec Unit
spec = around withBrowser do
  it "Should satisfy state benchmark" \browser -> do
    -- We can safely disregard 'Failed to parse CPU profile' log messages. This
    -- disables those logs from this point onwards in the program execution (all
    -- following benchmarks).
    liftEffect do
      Puppeteer.filterConsole
      catchException mempty (mkdir "test-results")

    let test = StateTest

    result <- compare browser 3 test

    result.hookAverage.averageFPS `shouldSatisfy` (_ > result.componentAverage.averageFPS / 3)
    result.hookAverage.averageHeap `shouldSatisfy` (_ < result.componentAverage.averageHeap * Kilobytes 3)

    liftEffect $ writeResult test result

  it "Should satisfy todo benchmark" \browser -> do
    let test = TodoTest

    result <- compare browser 3 test

    result.hookAverage.averageFPS `shouldSatisfy` (_ > result.componentAverage.averageFPS / 3)
    result.hookAverage.averageHeap `shouldSatisfy` (_ < result.componentAverage.averageHeap * Kilobytes 3)

    liftEffect $ writeResult test result

writeResult :: TestType -> ComparisonSummary -> Effect Unit
writeResult test { componentAverage, hookAverage } = do
  writePath "summary" $ encodeJson { componentAverage, hookAverage }
  where
  writePath label = writeTextFile UTF8 (mkPath label) <<< stringify
  mkPath label = "test-results/" <> testTypeToString test <> "-" <> label <> ".json"
