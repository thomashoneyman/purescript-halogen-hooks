module Example.Halogen.Components.Container (component) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Halogen.Components.Button as Button
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks

_button :: SProxy "button"
_button = SProxy

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hooks.component \{ slotToken } _ -> Hooks.do
  count /\ countId <- Hooks.useState 0
  buttonStatus /\ buttonStatusId <- Hooks.useState Nothing

  let
    handleButton (Button.Toggled enabled) = Just do
      when enabled do
        Hooks.modify_ countId (_ + 1)

    handleClick = Just do
      status <- Hooks.query slotToken _button unit (H.request Button.IsOn)
      Hooks.put buttonStatusId status

  Hooks.pure do
    HH.div_
      [ HH.slot _button unit Button.component unit handleButton
      , HH.p_
          [ HH.text $ "Button has been toggled 'on' " <> show count <> " time(s)" ]
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
