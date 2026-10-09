# Halogen Hooks Tests

Hooks are tested in two ways:

1. Behavior tests, which exercise the logic of each of the primitive Hooks in isolation and together to verify they behave as they should;
2. Performance tests, which ensure that the overhead incurred by Hooks is not too large and that changes to the library don't cause regressions.

Run `npm test` for behavioral/integration tests and `npm run test:performance`
for browser performance measurements, from the repository root. CI compiles both
targets and runs the behavioral suite; browser measurements remain a local check.

## Behavioral Tests

The `Hooks` directory contains tests that exercise the logic of the Hooks provided by this library. The `evalM` and `eval` functions allow you to call functions within a Hook, triggering new evaluations, and then use `readResult` to see the resulting Hook state and a log of what happened during the evaluations. If you are contributing a new test, use the existing tests as a template (especially `useState`, which is simple).

## Performance Tests

The `Performance` directory contains small apps that are run by Puppeteer. These tests measure the performance metrics that are observable via the Chrome developer tools, and output trace.json files that can be imported into the Chrome developer tools for more granular looks at performance.

These tests are meant to measure the overhead incurred by Hooks and aid in attempts to make the library more performant. **These tests are not typically reflective of real-world use, and large numbers don't mean poor performance in the real world. They are simply meant to measure whether internal changes have positive or negative performance implications.** Hooks tests are usually accompanied by the equivalent implementation using ordinary Halogen components as a reference.

Each Hooks release contains a snapshot, which is an average of several runs of a benchmark, which can be used to ensure regressions haven't occurred.

`npm ci` installs Puppeteer 25.13.0 and downloads its matching Chrome for Testing. On Linux,
the browser also needs the usual Chromium system libraries and a working sandbox.
The performance command uses Spago's CoreFn output, `purs-backend-es`, and esbuild,
then writes traces and summaries under `test-results/`. The two test cases verify
positive FPS, scripting time and peak heap measurements for every run, but do not
assert performance regression thresholds. Review results rather than treating a
passing run as proof of unchanged performance.

FPS now measures `requestAnimationFrame` cadence; scripting time is the change
in Chrome DevTools Protocol `ScriptDuration`. Heap sampling and full trace output
remain available. This replaces obsolete trace parsers that produced zero FPS
with current Chrome. Compiler, optimizer, browser and measurement changes make
historical snapshot deltas non-comparable. Do not interpret the generated `change`
files as regressions until a maintainer has reviewed and replaced those baselines.

`npm run snapshot` intentionally replaces the checked-in performance snapshots.
Only run it when updating those baselines, not as a migration validation step.
