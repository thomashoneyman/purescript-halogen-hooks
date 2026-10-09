module Example.Halogen.Basic.Button where

import Prelude

import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

component :: forall q i o m. H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  enabled /\ enabledId <- Hooks.useState false

  let
    label = if enabled then "On" else "Off"
    handleClick = Hooks.modify_ enabledId not

  Hooks.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> handleClick
      ]
      [ HH.text label ]
