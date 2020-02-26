module Example.Halogen.ComponentsInputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Halogen.ComponentsInputs.Display as Display
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

_display = SProxy :: SProxy "display"

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  count /\ countState <- Hook.useState 1

  let
    decrement = EH.modify_ countState (_ - 1)
    increment = EH.modify_ countState (_ + 1)

  Hook.pure do
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
