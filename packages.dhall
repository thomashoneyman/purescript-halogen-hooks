let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200716/packages.dhall sha256:c4683b4c4da0fd33e0df86fc24af035c059270dd245f68b79a7937098f6c6542

let overrides = { metadata = upstream.metadata // { version = "v0.13.6" } }

let additions =
      { halogen-storybook =
        { dependencies = [ "halogen", "routing", "foreign-object" ]
        , repo = "https://github.com/rnons/purescript-halogen-storybook.git"
        , version = "v1.0.0-rc.1"
        }
      }

in  upstream // overrides // additions
