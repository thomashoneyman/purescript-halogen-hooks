module Example.Halogen.Effects.Random where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks

type State = Maybe Number

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hooks.component \_ _ -> Hooks.do
  state /\ modifyState <- Hooks.useState Nothing

  let
    value = maybe "No number generated yet" show state

    handleClick = Just do
      newNumber <- H.liftEffect random
      modifyState \_ -> Just newNumber

  Hooks.pure do
    HH.div_
      [ HH.h1_
          [ HH.text "Random number" ]
      , HH.p_
          [ HH.text $ "Current value: " <> value ]
      , HH.button
          [ HE.onClick \_ -> handleClick ]
          [ HH.text "Generate new number" ]
      ]
