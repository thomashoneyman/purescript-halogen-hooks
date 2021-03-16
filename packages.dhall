let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210313/packages.dhall sha256:ba6368b31902aad206851fec930e89465440ebf5a1fe0391f8be396e2d2f1d87

let additions =
      { halogen-storybook =
        { dependencies = [ "halogen", "routing", "foreign-object" ]
        , repo = "https://github.com/rnons/purescript-halogen-storybook.git"
        , version = "7327247aea379d4582dad9f93be9749556d26c99"
        }
      }

in  upstream // additions
