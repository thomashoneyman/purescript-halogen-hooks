module Example.Halogen.Components.Container (component) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Example.Halogen.Components.Button as Button
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

_button :: SProxy "button"
_button = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  toggleCount /\ toggleCountState <- Hook.useState 0
  buttonStatus /\ buttonStatusState <- Hook.useState Nothing

  let
    handleButton (Button.Toggled _) = Just do
      EH.modify_ toggleCountState (_ + 1)

    handleClick = Just do
      EH.put buttonStatusState =<< EH.query _button unit (H.request Button.IsOn)

  Hook.pure do
    HH.div_
      [ HH.slot _button unit Button.component unit handleButton
      , HH.p_
          [ HH.text $ "Button has been toggled " <> show toggleCount <> " time(s)" ]
      , HH.p_
          [ HH.text $ fold
              [ "Last time I checked, the button was: "
              , maybe "(not checked yet)" (if _ then "on" else "off") buttonStatus
              , ". "
              ]
          , HH.button
              [ HE.onClick \_ -> handleClick ]
              [ HH.text "Check now" ]
          ]
      ]
