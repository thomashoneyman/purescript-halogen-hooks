# Halogen Hooks

[![CI](https://github.com/thomashoneyman/purescript-halogen-hooks/workflows/CI/badge.svg?branch=master)](https://github.com/thomashoneyman/purescript-halogen-hooks/actions?query=workflow%3ACI+branch%3Amaster)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-hooks.svg)](https://github.com/thomashoneyman/purescript-halogen-hooks/releases)
[![Latest package set](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-0lf69kxs4fbd.runkit.sh/halogen-hooks)](https://github.com/purescript/package-sets)

Reusable stateful logic for Halogen, inspired by [React Hooks](https://reactjs.org/docs/hooks-intro.html).

Hooks offer a simpler mental model for writing stateful code in Halogen. They are a better alternative to higher-order and renderless components and offer a more convenient way to write most ordinary components. They can be incrementally adopted and require no changes to existing components in your application.

> **Note:** Hooks can be used in production code today, but this library is very new. For that reason I don't recommend switching your entire application to Hooks right away. Instead, consider writing new components with Hooks or converting a few non-essential components before diving in all the way.

Learn more about Hooks:

1. [Read the blog post introducing Halogen Hooks](https://thomashoneyman.com/articles/introducing-halogen-hooks)
2. [Read the Halogen Hooks documentation](./docs)
3. [View the component and custom hook examples](./examples)

## Installation

You can install Halogen Hooks with Spago:

```sh
spago install halogen-hooks
```

If Halogen Hooks is not available in your package set, add it to your project's `packages.dhall` file:

```dhall
let additions =
  { halogen-hooks =
      { dependencies = [ "halogen", "indexed-monad", "record" ]
      , repo = "https://github.com/thomashoneyman/purescript-halogen-hooks.git"
      , version = "master"
      }
  }
```

## Quick start

All types and functions available in Hooks are exported from the `Halogen.Hooks` module, so you can access the entirety of Hooks with one import:

```purs
import Halogen.Hooks as Hooks
```

Halogen Hooks uses an indexed free monad to ensure Hooks are always run in the same order. For that reason you should use `do`, `bind`, and `pure` as qualified imports from the `Halogen.Hooks` module.

This code replicates the Halogen basic button example which renders a count that is incremented on click:

```purs
myComponent = Hooks.component \_ input -> Hooks.do
  count /\ countId <- Hooks.useState 0

  Hooks.pure do
    HH.button
      [ HE.onClick \_ -> Just $ Hooks.modify_ countId (_ + 1) ]
      [ HH.text $ show count ]
```

## Documentation

There are several resources to help you learn about Hooks:

- **Introducing Hooks**

  The [blog post which introduces Halogen Hooks](https://thomashoneyman.com/articles/introducing-halogen-hooks) helps you understand what Hooks are, why they're important, and how to use them.

- **The Hooks Documentation**

  The [documentation directory](./docs) in this repository contains a deep dive into Hooks and how to use them, including an API reference for each hook and a FAQ (feel free to open an issue if you have questions of your own you'd like answered).

- **Hooks Examples**

  The [Hooks examples directory](./examples) in this repository replicates a number of example components from the official Halogen repository and demonstrates several custom Hooks you could write for yourself. Feel free to use these in your own code!

## Contributing

You can contribute to Halogen Hooks in many ways:

- If you encounter an issue or have a question, please open an issue on the repository and I will work with you to resolve or answer it.

- If you have suggestions for the documentation, please open an issue or pull request.

- If you would like to contribute code to the library, please make sure to read the [contributor's guidelines](./.github/CONTRIBUTING.md). In general, pull requests which improve the performance and ergonomics of the library are welcome, but I am unlikely to expand the core API or merge additions which I believe make the library more complicated to use.

- If you have written a custom hook, a tutorial, or another resource on top of Halogen Hooks I encourage you to share it on the [PureScript Discourse](https://discourse.purescript.org)! Implementing your own hooks and writing your own guides and resources are a great way to help Halogen Hooks grow.

### Developer Environments

If you would like to contribute code to Halogen Hooks, you can enter a development environment with common PureScript tooling available at the same versions I use via Nix. Just run this command in the root of the project:

```sh
nix-shell
```
