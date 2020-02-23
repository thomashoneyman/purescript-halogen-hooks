module Example.Effects.Random where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

type State = Maybe Number

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  state /\ _state <- Hook.useState Nothing

  let
    value = maybe "No number generated yet" show state

    handleClick = Just do
      newNumber <- H.liftEffect random
      EH.put _state (Just newNumber)

  Hook.pure do
    HH.div_
      [ HH.h1_
          [ HH.text "Random number" ]
      , HH.p_
          [ HH.text $ "Current value: " <> value ]
      , HH.button
          [ HE.onClick \_ -> handleClick ]
          [ HH.text "Generate new number" ]
      ]
