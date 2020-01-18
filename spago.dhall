{ name = "halogen-hooks"
, dependencies = [ "console", "debug", "effect", "halogen", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "example/**/*.purs" ]
}
