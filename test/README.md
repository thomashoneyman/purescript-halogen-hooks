# Testing Halogen Hooks

Since Halogen is by nature effectful and works only in the browser, testing this code requires some manual work. This directory exists to make that easier.

There are two ways we can test our code. We can set up the hooks so that each runs in its own component (i.e. isolated). We can also set up the hooks, so that all of them run in the same component (i.e. shared).

Run the below code (which one depends on the which approach you want to take), copy the file path to the `index.html` file, and open it in your browser. Then, manually try things out to verify that the code works correctly.
```bash
# Isolated approach
spago -x test/test.dhall bundle-app --main Test.Isolated --to test/app.js

# Shared approach
spago -x test/test.dhall bundle-app --main Test.Shared --to test/app.js
```
