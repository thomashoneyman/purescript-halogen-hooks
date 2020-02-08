module Example.Basic.Button where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hook as Hook

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = Hook.component Hook.do
  state /\ setState <- Hook.useState \_ -> { enabled: false }

  let label = if state.enabled then "On" else "Off"

  Hook.pure do
    HH.button
      [ HP.title label 
      , HE.onClick \_ -> Just $ setState { enabled: not state.enabled } 
      ]
      [ HH.text label ]
  