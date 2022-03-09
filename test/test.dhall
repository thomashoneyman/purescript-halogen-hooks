let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "aff-promise"
        , "avar"
        , "argonaut-codecs"
        , "argonaut-core"
        , "either"
        , "console"
        , "integers"
        , "node-buffer"
        , "node-fs"
        , "node-path"
        , "nullable"
        , "spec"
        , "strings"
        , "random"
        ]
  , sources =
      conf.sources #
        [ "test/**/*.purs"
        ]
  }
