module Test.Setup.Performance.Measure where

import Prelude hiding (compare)

import Data.Array as Array
import Data.Foldable (foldl, traverse_)
import Data.Maybe (fromJust)
import Data.Newtype (over)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Node.Path (resolve)
import Partial.Unsafe (unsafePartial)
import Test.Setup.Performance.App (completedSuffix, testsId)
import Test.Setup.Performance.App as App
import Test.Setup.Performance.Puppeteer (Browser, FilePath(..), Kilobytes(..), Milliseconds(..))
import Test.Setup.Performance.Puppeteer as Puppeteer

type PerformanceSummary =
  { averageFPS :: Int
  , elapsedTime :: Milliseconds
  , heapUsed :: Kilobytes
  }

-- | Bracket test runs by supplying a new browser to each one
withBrowser :: (Browser -> Aff Unit) -> Aff Unit
withBrowser = bracket Puppeteer.launch Puppeteer.closeBrowser

data TestType = StateTest

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
compare :: Browser -> TestType -> Aff { hook :: PerformanceSummary, component :: PerformanceSummary }
compare browser = case _ of
  StateTest -> do
    hook <- measure' App.StateHook
    component <- measure' App.StateComponent
    pure { hook, component }
  where
  measure' = measure browser

measure :: Browser -> App.Test -> Aff PerformanceSummary
measure browser test = do
  page <- Puppeteer.newPage browser

  path <- liftEffect $ resolve [] "test/test.html"
  Puppeteer.goto page ("file://" <> path)
  void $ Puppeteer.waitForSelector page (prependHash testsId)

  let selector = prependHash (App.testToString test)

  Puppeteer.enableHeapProfiler page
  Puppeteer.collectGarbage page
  Puppeteer.startTrace page (FilePath ("test/" <> (App.testToString test <> "-trace.json")))
  initialPageMetrics <- Puppeteer.pageMetrics page

  -- Run the test to completion
  Puppeteer.waitForSelector page selector >>= traverse_ Puppeteer.click
  _ <- Puppeteer.waitForSelector page (selector <> completedSuffix)

  -- Collect garbage again before collecting heap measurements; without
  -- this occasional spikes at the end of tests will throw off results.
  Puppeteer.collectGarbage page

  finalPageMetrics <- Puppeteer.pageMetrics page
  trace <- Puppeteer.stopTrace page
  Puppeteer.closePage page

  mbModel <- Puppeteer.getPerformanceModel trace

  let metrics = finalPageMetrics - initialPageMetrics

  pure
    { averageFPS: Puppeteer.getAverageFPS $ unsafePartial $ fromJust mbModel
    , heapUsed: metrics.heapUsed
    , elapsedTime: metrics.timestamp
    }

prependHash :: String -> String
prependHash str = "#" <> str

average :: Array PerformanceSummary -> PerformanceSummary
average summaries = do
  let
    summary = foldl (+) zero summaries
    total = Array.length summaries

  { averageFPS: summary.averageFPS / total
  , elapsedTime: over Milliseconds (_ / total) summary.elapsedTime
  , heapUsed: over Kilobytes (_ / total) summary.heapUsed
  }
