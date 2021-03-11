module Example.Halogen.Components.Container (component) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Halogen.Components.Button as Button
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

_button :: Proxy "button"
_button = Proxy

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = Hooks.component \{ slotToken } _ -> Hooks.do
  count /\ countId <- Hooks.useState 0
  buttonStatus /\ buttonStatusId <- Hooks.useState Nothing

  let
    handleButton (Button.Toggled enabled) = do
      when enabled do
        Hooks.modify_ countId (_ + 1)

    handleClick = do
      status <- Hooks.request slotToken _button unit Button.IsOn
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
