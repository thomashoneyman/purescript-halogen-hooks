module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Basic.Button as Basic
import Example.Components.Container as Components
import Example.Components.Inputs.Container as Inputs.Container
import Example.Effects.Random as Random
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hook as Hook
import Halogen.Storybook (Stories, runStorybook)

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories: examples
    , logo: Just $ HH.text "Halogen Hooks Examples"
    }

examples :: Stories Aff
examples =
  Object.fromFoldable
    [ "" /\ index
    , "Halogen|Basic" /\ Basic.component
    , "Halogen|Components" /\ Components.component
    , "Halogen|Effect: Random" /\ Random.component
    , "Halogen|Inputs" /\ Inputs.Container.component
    ]
  where
  index :: forall q i o m. H.Component HH.HTML q i o m
  index = Hook.component \_ -> Hook.pure do
    HH.div_
      [ HH.h1_
        [ HH.text "Halogen Hooks" ]
      , HH.p_
        [ HH.text "See the Halogen Hooks "
        , HH.a
          [ HP.href "https://github.com/thomashoneyman/purescript-halogen-hooks" ]
          [ HH.text "README" ]
        , HH.text " for details."
        ]
      ]
