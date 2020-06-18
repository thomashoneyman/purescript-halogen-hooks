let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200615/packages.dhall sha256:5d0cfad9408c84db0a3fdcea2d708f9ed8f64297e164dc57a7cf6328706df93a

let overrides = {=}

let additions =
      { halogen-storybook =
        { dependencies = [ "halogen", "routing", "foreign-object" ]
        , repo = "https://github.com/rnons/purescript-halogen-storybook.git"
        , version = "v1.0.0-rc.1"
        }
      }

in  upstream // overrides // additions
