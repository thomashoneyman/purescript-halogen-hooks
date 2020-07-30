let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "aff-promise"
        , "argonaut-codecs"
        , "argonaut-core"
        , "debug"
        , "node-fs"
        , "spec"
        , "random"
        , "record"
        ]
  , sources =
      conf.sources #
        [ "test/**/*.purs"
        ]
  }
