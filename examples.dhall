let conf = ./spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "argonaut"
        , "console"
        , "debug"
        , "effect"
        , "halogen-storybook"
        , "psci-support"
        , "random"
        ]
  , sources =
      conf.sources #
        [ "examples/**/*.purs"
        ]
  }
