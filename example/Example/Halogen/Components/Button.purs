module Example.Halogen.Components.Button (Slot, Query(..), Message(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hook as Hook

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = Toggled Boolean

component :: forall i m. H.Component HH.HTML Query i Message m
component = Hook.componentWithQuery \queryToken _ -> Hook.do
  enabled /\ enabledState <- Hook.useState false

  Hook.useQuery queryToken case _ of
    IsOn reply -> do
      isEnabled <- EH.get enabledState
      pure (Just (reply isEnabled))

  let
    label = if enabled then "On" else "Off"

    handleClick = Just do
      isEnabled <- EH.modify enabledState not
      EH.raise (Toggled isEnabled)

  Hook.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> handleClick
      ]
      [ HH.text label ]
