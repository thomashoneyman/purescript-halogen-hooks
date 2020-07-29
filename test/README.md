# Halogen Hooks Tests

Hooks are tested in two ways:

1. Behavior tests, which exercise the logic of each of the primitive Hooks in isolation and together to verify they behave as they should;
2. Performance tests, which ensure that the overhead incurred by Hooks is not too large and that changes to the library don't cause regressions.

The `test` command exercises both tests.

## Behavioral Tests

The `Hooks` directory contains tests that exercise the logic of the Hooks provided by this library. The `evalM` and `eval` functions allow you to call functions within a Hook, triggering new evaluations, and then use `readResult` to see the resulting Hook state and a log of what happened during the evaluations. If you are contributing a new test, use the existing tests as a template (especially `useState`, which is simple).

## Performance Tests

The `Performance` directory contains small apps that are run by Puppeteer. These tests measure the performance metrics that are observable via the Chrome developer tools, and output trace.json files that can be imported into the Chrome developer tools for more granular looks at performance.

These tests are meant to measure the overhead incurred by Hooks and aid in attempts to make the library more performant. **These tests are not typically reflective of real-world use, and large numbers don't mean poor performance in the real world. They are simply meant to measure whether internal changes have positive or negative performance implications.** Hooks tests are usually accompanied by the equivalent implementation using ordinary Halogen components as a reference.

Each Hooks release contains a snapshot, which is an average of several runs of a benchmark, which can be used to ensure regressions haven't occurred.
