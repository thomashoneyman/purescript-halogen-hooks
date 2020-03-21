module Example.Halogen.Basic.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

component :: forall q i o m. H.Component HH.HTML q i o m
component = Hooks.component \_ -> Hooks.do
  enabled /\ enabledState <- Hooks.useState false

  let
    label = if enabled then "On" else "Off"
    handleClick = Hooks.modify_ enabledState not

  Hooks.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just handleClick
      ]
      [ HH.text label ]
