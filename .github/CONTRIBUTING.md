# Contribution

Thank you for your interest in contributing to Halogen Hooks! This is a short, sweet introduction to help you get started contributing.

## Getting Started

### Do I belong here?

Everyone is welcome! People of all experience levels can join, begin contributing, and feel comfortable and safe making mistakes. People of all backgrounds belong here so long as they treat others with dignity and respect and do not harass or belittel others.

### What is the correct way to ask a question?

Feel free to ask questions by opening an issue on the relevant library. Maintainers are also active on:

- The [PureScript Discourse](https://discourse.purescript.org) (the most popular option and best for detailed questions)
- The [PureScript Discord server](https://purescript.org/chat) (best for quick, informal questions)

### I'd like to help, how do I pick something to work on?

Any open issue that is not yet assigned to someone is good to work on! If it's your first time contributing it's probably best to pick an issue marked `good first issue`. In general:

1. Issues marked `good first issue` are good for beginners and/or new contributors to the library.
2. Issues marked `help wanted` signal that anyone can take the issue and it's a desired addition to the library.
3. Issues marked `document me` are requests for documentation and are often a great first issue to take on.

The easiest way you can help is by contributing documentation, whether via looking for issues marked `document me` or by adding new documentation of your own. If you'd like to contribute documentation I suggest [reading about the four kinds of documentation](https://documentation.divio.com).

### How big should my contribution be?

Your contribution can be as small as copypasting instructions from an issue into the project documentation! Everything is welcome, including very small changes and quality of life improvements.

In general, pull requests which improve the performance and ergonomics of the library are welcome, but I am unlikely to expand the core API or merge additions which I believe make the library more complicated to use. If you would like to contribute a particularly large or a breaking change, you may want to open an issue proposing the change before you implement it. That helps me ensure your time is not wasted.

## Contributing Code

### Developer Environments

Use Node 24.21.0 LTS and npm 11.19.0. Install the locked npm tools,
including PureScript 0.15.16 and Spago 1.0.4, from the repository root:

```sh
npm ci
npm run build
npm run build:examples
npm test
npm run bundle:examples
npm run test:performance
```

`npm test` runs behavioral/integration tests, not browser performance tests.
`npm run build` also compiles `test/**/*.purs`; `build:test` is retained as an
explicit alias. Examples are a workspace package under `examples/src`, and use
the local library. See [test documentation](../test/README.md) for browser setup.

Alternatively, with Nix 2.18 or newer, run `nix develop`, then the same npm
commands. The flake pins a current nixos-26.05 package set and supplies Node 24
and Git. PureScript, Spago, purs-tidy, and esbuild all come from `npm ci`, matching
the npm-based CI toolchain. Linux x86_64 is verified; macOS and NixOS are not.

Registry package set `0.0.1` preserves the original dependency resolutions.
Production bounds preserve the published registry manifest for `halogen-hooks`
0.6.3, rather than inferring new bounds from that one resolution. Legacy Dhall
declared names, not version ranges. No published bound is narrowed or widened by
this migration; in particular Aff remains `>=7.1.0 <9.0.0` and Parallel remains
`>=6.0.0 <8.0.0`. Toolchain upgrades do not by themselves establish compatibility
with every admitted dependency version. Commit both `package-lock.json` and
`spago.lock` when dependencies change; avoid `--ensure-ranges` when preserving
published bounds.

Run `npm audit` to check all development tooling too. As of this upgrade, the
remaining high-severity advisory is
[GHSA-vfj7-8cjw-p6xm](https://github.com/advisories/GHSA-vfj7-8cjw-p6xm) in
`spago -> micromatch -> braces`, with no patched upstream release. npm reports
three affected packages for that chain. The CI audit step intentionally fails
until the dependency is fixed; no audit suppression or forced downgrade is used.

This configuration does not select a new release version or enable publication.
The library API and published bounds are unchanged, so these edits alone do not
require a breaking dependency-bound release; final version selection remains a
maintainer decision. Browser/compiler upgrades require newly reviewed performance
baselines, not comparison with the historical snapshots.
Before a release, review compatibility bounds, license/location/version metadata,
and the registry-only production dependency closure. Do not run `spago publish`
as a check: it has no dry-run mode and can push a tag.

### Proposing changes

If you would like to contribute code, tests, or documentation, please feel free to open a pull request for small changes. For large changes we recommend you first open an issue to propose your change.
