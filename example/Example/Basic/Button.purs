module Example.Basic.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hook as Hook

component :: forall q i o m. H.Component HH.HTML q i o m
component = Hook.component \_ _ -> Hook.do
  state /\ _state <- Hook.useState { enabled: false }

  let
    label = if state.enabled then "On" else "Off"

    handleClick = Just do
      EH.modify_ _state \st -> { enabled: not st.enabled }

  Hook.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> handleClick
      ]
      [ HH.text label ]
