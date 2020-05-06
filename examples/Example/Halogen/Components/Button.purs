module Example.Halogen.Components.Button (Slot, Query(..), Message(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Slot = H.Slot Query Message

data Query a = IsOn (Boolean -> a)

data Message = Toggled Boolean

type Tokens = Hooks.ComponentTokens Query () Message

component :: forall i m. MonadEffect m => H.Component HH.HTML Query i Message m
component = Hooks.component \(tokens :: Tokens) _ -> Hooks.do
  enabled /\ modifyEnabled <- Hooks.useState false

  Hooks.useQuery tokens.queryToken case _ of
    IsOn reply -> do
      pure (Just (reply enabled))

  let
    label = if enabled then "On" else "Off"

    handleClick = Just do
      let enabled' = not enabled
      modifyEnabled (const enabled')
      Hooks.raise tokens.outputToken (Toggled enabled')

  Hooks.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> handleClick
      ]
      [ HH.text label ]
