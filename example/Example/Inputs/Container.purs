module Example.Components.Inputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

_display = SProxy :: SProxy "display"

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  state /\ _state <- Hook.useState 1

  let
    decrement = Just do
      EH.modify_ _state (_ - 1)

    increment = Just do
      EH.modify_ _state (_ + 1)

  Hook.pure do
    HH.div_
      [ HH.ul_
          [ HH.slot _display 1 Display.component state absurd
          , HH.slot _display 2 Display.component (state * 2) absurd
          , HH.slot _display 3 Display.component (state * 3) absurd
          , HH.slot _display 4 Display.component (state * 10) absurd
          , HH.slot _display 5 Display.component (state * state) absurd
          ]
      , HH.button
          [ HE.onClick \_ -> decrement ]
          [ HH.text "- 1"]
      , HH.button
          [ HE.onClick \_ -> increment ]
          [ HH.text "+ 1"]
      ]
