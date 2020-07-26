let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "aff-promise"
        , "argonaut-core"
        , "debug"
        , "node-fs"
        , "spec"
        , "record"
        ]
  , sources =
      conf.sources #
        [ "test/**/*.purs"
        ]
  }
