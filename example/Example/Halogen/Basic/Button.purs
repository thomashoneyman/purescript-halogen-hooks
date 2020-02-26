module Example.Halogen.Basic.Button where

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
  enabled /\ enabledState <- Hook.useState false

  let
    label = if enabled then "On" else "Off"
    handleClick = EH.modify_ enabledState not

  Hook.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just handleClick
      ]
      [ HH.text label ]
