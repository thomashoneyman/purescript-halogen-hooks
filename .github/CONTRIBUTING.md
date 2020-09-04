# Contribution

Thank you for your interest in contributing to Halogen Hooks! This is a short, sweet introduction to help you get started contributing.

## Getting Started

### Do I belong here?

Everyone is welcome! People of all experience levels can join, begin contributing, and feel comfortable and safe making mistakes. People of all backgrounds belong here so long as they treat others with dignity and respect and do not harass or belittel others.

### What is the correct way to ask a question?

Feel free to ask questions by opening an issue on the relevant library. Maintainers are also active on:

- The [PureScript Discourse](https://discourse.purescript.org) (the most popular option and best for detailed questions)
- The [Functional Programming Slack](https://functionalprogramming.slack.com) ([link to join](https://fpchat-invite.herokuapp.com)!) in the `#purescript` and `#purescript-beginners` channels (best for quick, informal questions)

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

If you would like to contribute code to Halogen Hooks, you can enter a development environment with common PureScript tooling available at the same versions I use via Nix. Just run this command in the root of the project:

```sh
nix-shell
```

You will also need to install development dependencies from the `package.json` file if you are working on the tests. That file also contains scripts helpful for building tests and examples.

### Proposing changes

If you would like to contribute code, tests, or documentation, please feel free to open a pull request for small changes. For large changes we recommend you first open an issue to propose your change.
