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
component = Hook.component \_ -> Hook.do
  state /\ id <- Hook.useState { enabled: false }
  state2 /\ id2 <- Hook.useState true

  let
    label = if state.enabled then "On" else "Off"

    handleClick = do
      EH.modify_ id \st -> { enabled: not st.enabled }
      EH.modify_ id2 \_ -> false

  Hook.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just handleClick
      ]
      [ HH.text label ]
