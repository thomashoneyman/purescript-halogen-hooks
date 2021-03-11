let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210309/packages.dhall sha256:585332a8a11c6420d7287943f81bc2121746cdd352f2cf3f5ecf65053f2afcd3

let overrides = { metadata = upstream.metadata // { version = "v0.14.0" } }

let additions =
      { halogen-storybook =
        { dependencies = [ "halogen", "routing", "foreign-object" ]
        , repo = "https://github.com/CarstenKoenig/purescript-halogen-storybook.git"
        , version = "purescript-0.14"
        }
      }

in  upstream // overrides // additions
