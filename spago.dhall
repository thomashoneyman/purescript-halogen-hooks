{ name = "halogen-hooks"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "halogen"
    , "halogen-storybook"
    , "psci-support"
    , "random"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "example/**/*.purs" ]
}
