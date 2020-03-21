module Example.Halogen.Components.Button (Slot, Query(..), Message(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = Toggled Boolean

component :: forall i m. H.Component HH.HTML Query i Message m
component = Hooks.componentWithQuery \queryToken _ -> Hooks.do
  enabled /\ enabledState <- Hooks.useState false

  Hooks.useQuery queryToken case _ of
    IsOn reply -> do
      isEnabled <- Hooks.get enabledState
      pure (Just (reply isEnabled))

  let
    label = if enabled then "On" else "Off"

    handleClick = Just do
      isEnabled <- Hooks.modify enabledState not
      Hooks.raise (Toggled isEnabled)

  Hooks.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> handleClick
      ]
      [ HH.text label ]
