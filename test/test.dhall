let config = ../spago.dhall
in config //
     { dependencies = config.dependencies # [ "random" ]
     , sources = config.sources # [ "test/**/*.purs" ]
     }
