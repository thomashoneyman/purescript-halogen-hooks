let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "debug"
        , "spec"
        , "record"
        ]
  , sources =
      conf.sources #
        [ "test/**/*.purs"
        ]
  }
