let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200911-2/packages.dhall sha256:872c06349ed9c8210be43982dc6466c2ca7c5c441129826bcb9bf3672938f16e

let overrides =
      { metadata = upstream.metadata // { version = "v0.13.6" }
      , halogen = upstream.halogen //
          { repo = "https://github.com/thomashoneyman/purescript-halogen.git"
          , version = "freed"
          }
      }

let additions =
      { halogen-storybook =
        { dependencies = [ "halogen", "routing", "foreign-object" ]
        , repo = "https://github.com/rnons/purescript-halogen-storybook.git"
        , version = "v1.0.0-rc.1"
        }
      }

in  upstream // overrides // additions
