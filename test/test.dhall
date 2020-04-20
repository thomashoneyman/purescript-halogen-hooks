let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "debug"
        , "spec"
        ]
  , sources =
      conf.sources #
        [ "test/**/*.purs"
        ]
  }
