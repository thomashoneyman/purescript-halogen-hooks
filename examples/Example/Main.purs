module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Halogen.Basic.Button as Halogen.Basic
import Example.Halogen.Components.Container as Halogen.Components.Container
import Example.Halogen.ComponentsInputs.Container as Halogen.ComponentsInputs.Container
import Example.Halogen.Effects.Random as Halogen.Effects.Random
import Example.Halogen.InputRef.Component as Halogen.InputRef
import Example.Hooks.Components as HookComponents
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
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

    -- Examples of writing hooks
    , "Hooks|useWindowWidth" /\ HookComponents.windowWidth
    , "Hooks|usePreviousValue" /\ HookComponents.previousValue
    , "Hooks|useLocalStorage" /\ HookComponents.localStorage
    , "Hooks|useDebouncer" /\ HookComponents.debouncer
    , "Hooks|useGet" /\ HookComponents.get

    -- Examples from the existing Halogen documentation
    , "Halogen|Basic" /\ Halogen.Basic.component
    , "Halogen|Components" /\ Halogen.Components.Container.component
    , "Halogen|Components: Inputs" /\ Halogen.ComponentsInputs.Container.component
    , "Halogen|Effects: Random" /\ Halogen.Effects.Random.component

    -- Not quite the Ace component, but the code for Ace is very old and I had
    -- trouble getting it to run properly. Still, the point is to demonstrate refs
    -- , subscriptions, and queries, and the latter two are covered elsewhere.
    , "Halogen|InputRef" /\ Halogen.InputRef.component
    ]
  where
  index :: forall q i o m. H.Component HH.HTML q i o m
  index = Hooks.component \_ _ -> Hooks.pure do
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
