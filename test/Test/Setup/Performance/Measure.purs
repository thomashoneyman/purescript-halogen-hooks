module Test.Setup.Performance.Measure where

import Prelude hiding (compare)

import Data.Array (replicate)
import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Effect.Aff (Aff, bracket, delay, error, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Node.Path (resolve)
import Partial.Unsafe (unsafePartial)
import Test.Performance.Test (Test(..), completedSuffix, startSuffix, testToString)
import Test.Performance.Todo.Shared (addNewId, checkId, editId, saveId)
import Test.Setup.Performance.Puppeteer (Browser, FilePath(..), Kilobytes(..), Milliseconds(..), Page)
import Test.Setup.Performance.Puppeteer as Puppeteer

type PerformanceSummary =
  { averageFPS :: Int
  , elapsedTime :: Milliseconds
  , heapUsed :: Kilobytes
  }

type ComparisonSummary =
  { hookResults :: Array PerformanceSummary
  , hookAverage :: PerformanceSummary
  , componentResults :: Array PerformanceSummary
  , componentAverage :: PerformanceSummary
  }

-- | Bracket test runs by supplying a new browser to each one
withBrowser :: (Browser -> Aff Unit) -> Aff Unit
withBrowser = bracket Puppeteer.launch Puppeteer.closeBrowser

data TestType = StateTest | TodoTest

compare :: Browser -> Int -> TestType -> Aff ComparisonSummary
compare browser n testType = do
  let runs = replicate n (compareOnce browser testType)
  results <- for runs (delay (Aff.Milliseconds 100.0) *> _)

  let
    hookResults = map _.hook results
    componentResults = map _.component results
    hookAverage = average hookResults
    componentAverage = average componentResults

  pure { hookResults, hookAverage, componentResults, componentAverage }

compareOnce :: Browser -> TestType -> Aff { hook :: PerformanceSummary, component :: PerformanceSummary }
compareOnce browser = case _ of
  StateTest -> do
    hook <- measure browser StateHook
    component <- measure browser StateComponent
    pure { hook, component }

  TodoTest -> do
    hook <- measure browser TodoHook
    component <- measure browser TodoComponent
    pure { hook, component }

measure :: Browser -> Test -> Aff PerformanceSummary
measure browser test = do
  page <- Puppeteer.newPage browser

  path <- liftEffect $ resolve [] "test/test.html"
  Puppeteer.goto page ("file://" <> path)

  -- Prepare by selecting the test to mount
  let selector = prependHash (testToString test)
  mbTestElem <- Puppeteer.waitForSelector page selector

  -- Prepare for the test by collecting garbage (for more accurate heap usage
  -- metrics) and starting metrics collection
  Puppeteer.enableHeapProfiler page
  Puppeteer.collectGarbage page
  Puppeteer.startTrace page (FilePath ("test/" <> (testToString test <> "-trace.json")))
  initialPageMetrics <- Puppeteer.pageMetrics page

  -- Run the test to completion
  for_ mbTestElem Puppeteer.click
  runScriptForTest page test

  -- Collect garbage again before collecting heap measurements; without this,
  -- occasional spikes at the end of tests will throw off results.
  Puppeteer.collectGarbage page

  finalPageMetrics <- Puppeteer.pageMetrics page
  trace <- Puppeteer.stopTrace page
  Puppeteer.closePage page

  -- TODO: Is it possible to filter this only to script execution? The trace
  -- contains some dead time that affects results by including Puppeteer overhead
  -- and downtime.
  mbModel <- Puppeteer.getPerformanceModel trace

  let metrics = finalPageMetrics - initialPageMetrics

  pure
    { averageFPS: Puppeteer.getAverageFPS $ unsafePartial $ fromJust mbModel
    , heapUsed: metrics.heapUsed
    , elapsedTime: metrics.timestamp
    }

-- TODO: Replace query selectors
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
runScriptForTest :: Page -> Test -> Aff Unit
runScriptForTest page test = let selector = prependHash (testToString test) in case test of
  _ | test == StateHook || test == StateComponent -> do
        n <- Puppeteer.waitForSelector page (selector <> startSuffix)
        for_ n Puppeteer.click
        void $ Puppeteer.waitForSelector page (selector <> completedSuffix)

    | test == TodoHook || test == TodoComponent -> do
        addNew <- Puppeteer.waitForSelector page (prependHash addNewId)
        for_ addNew Puppeteer.click

        check0 <- Puppeteer.waitForSelector page (prependHash $ checkId 0)
        for_ check0 Puppeteer.click
        check1 <- Puppeteer.waitForSelector page (prependHash $ checkId 1)
        for_ check1 Puppeteer.click

        Puppeteer.focus page (prependHash $ editId 5)
        Puppeteer.typeWithKeyboard page "is so fun"
        save5 <- Puppeteer.waitForSelector page (prependHash $ saveId 5)
        for_ save5 Puppeteer.click

        for_ check0 Puppeteer.click
        for_ check1 Puppeteer.click

  _ ->
    throwError $ error "Impossible!!!"

prependHash :: String -> String
prependHash str = "#" <> str

average :: Array PerformanceSummary -> PerformanceSummary
average summaries = do
  let
    summary = foldl (+) zero summaries
    total = Array.length summaries

  { averageFPS: summary.averageFPS / total
  , elapsedTime: summary.elapsedTime / Milliseconds total
  , heapUsed: summary.heapUsed / Kilobytes total
  }
