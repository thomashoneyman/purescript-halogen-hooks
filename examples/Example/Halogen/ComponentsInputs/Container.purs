module Example.Halogen.ComponentsInputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Halogen.ComponentsInputs.Display as Display
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks

_display = SProxy :: SProxy "display"

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hooks.component \_ _ -> Hooks.do
  count /\ countState <- Hooks.useState 1

  let
    decrement = Hooks.modify_ countState (_ - 1)
    increment = Hooks.modify_ countState (_ + 1)

  Hooks.pure do
    HH.div_
      [ HH.ul_
          [ HH.slot _display 1 Display.component count absurd
          , HH.slot _display 2 Display.component (count * 2) absurd
          , HH.slot _display 3 Display.component (count * 3) absurd
          , HH.slot _display 4 Display.component (count * 10) absurd
          , HH.slot _display 5 Display.component (count * count) absurd
          ]
      , HH.button
          [ HE.onClick \_ -> Just decrement ]
          [ HH.text "- 1"]
      , HH.button
          [ HE.onClick \_ -> Just increment ]
          [ HH.text "+ 1"]
      ]
