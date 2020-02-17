module Example.Basic.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hook as Hook

component :: forall q i o m. Functor m => H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  state /\ setState <- Hook.useState { enabled: false }

  let label = if state.enabled then "On" else "Off"

  Hook.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just $ setState { enabled: not state.enabled }
      ]
      [ HH.text label ]
