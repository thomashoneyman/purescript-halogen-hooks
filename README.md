# Halogen Hooks

[![CI](https://github.com/thomashoneyman/purescript-halogen-hooks/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-hooks/actions?query=workflow%3ACI+branch%3Amain)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-hooks.svg)](https://github.com/thomashoneyman/purescript-halogen-hooks/releases)

Reusable stateful logic for [Halogen](https://github.com/purescript-halogen/purescript-halogen/), inspired by [React Hooks](https://reactjs.org/docs/hooks-intro.html).

Hooks offer a simpler mental model for writing stateful code in Halogen. They are a better alternative to higher-order and renderless components and offer a more convenient way to write most ordinary components. They can be incrementally adopted and require no changes to existing components in your application.

> **Note:** Hooks can be used in production code today, but Hooks are not as performant as regular Halogen components. A Hooks-based component is typically 1.5x slower and uses 1.75x the memory of an equivalent Halogen component. In most use cases this performance difference is not noticeable, but keep this in mind if you are using Hooks in a large, performance-sensitive application. Please feel free to contribute performance improvements!

Learn more about Hooks:

1. [Read the blog post introducing Halogen Hooks](https://thomashoneyman.com/articles/introducing-halogen-hooks)
2. [Read the Halogen Hooks documentation](./docs)
3. [View the component and custom hook examples](./examples)
4. [View the Hooks recipes in the PureScript Cookbook](https://github.com/JordanMartinez/purescript-cookbook)

## Installation

You can install Halogen Hooks with Spago:

```sh
spago install halogen-hooks
```

If Halogen Hooks is not available in your package set, add it to your project's `packages.dhall` file:

```dhall
let additions =
  { halogen-hooks =
      { dependencies = [ "halogen" ]
      , repo = "https://github.com/thomashoneyman/purescript-halogen-hooks.git"
      , version = "main"
      }
  }
```

## Quick start

All types and functions available in Hooks are exported from the `Halogen.Hooks` module, so you can access the entirety of Hooks with one import:

```purs
import Halogen.Hooks as Hooks
```

Halogen Hooks ensures Hooks are always evaluated in the same order. For that reason you should use `do`, `bind`, and `pure` as qualified imports from the `Halogen.Hooks` module. You will see compile-time errors if you forget!

This code replicates the Halogen basic button example which renders a count that is incremented on click:

```purs
myComponent = Hooks.component \_ input -> Hooks.do
  count /\ countId <- Hooks.useState 0

  Hooks.pure do
    HH.button
      [ HE.onClick \_ -> Hooks.modify_ countId (_ + 1) ]
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

- If you would like to contribute code to the library, please make sure to read the [Hooks Internals](./docs/09-Hooks-Internals.md) documentation and the [Contributors Guidelines](./.github/CONTRIBUTING.md), which includes advice on setting up your development environment.

- If you have written a custom hook, a tutorial, or another resource on top of Halogen Hooks I encourage you to share it on the [PureScript Discourse](https://discourse.purescript.org)! Implementing your own hooks and writing your own guides and resources are a great way to help Halogen Hooks grow.
