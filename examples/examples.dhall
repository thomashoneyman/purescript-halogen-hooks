let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "argonaut"
        , "avar"
        , "datetime"
        , "effect"
        , "either"
        , "halogen-storybook"
        , "profunctor-lenses"
        , "random"
        , "web-events"
        , "web-storage"
        ]
  , sources =
      conf.sources #
        [ "examples/**/*.purs"
        ]
  }
