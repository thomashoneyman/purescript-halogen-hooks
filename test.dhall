let conf = ./spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "console"
        , "debug"
        , "effect"
        , "halogen-storybook"
        , "psci-support"
        , "random"
        ]
  , sources =
      conf.sources #
        [ "example/**/*.purs"
        ]
  }
