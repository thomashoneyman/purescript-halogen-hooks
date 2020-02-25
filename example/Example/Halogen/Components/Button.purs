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

type State = { enabled :: Boolean }

initialState :: State
initialState = { enabled: false }

component :: forall i m. H.Component HH.HTML Query i Message m
component = Hook.componentWithQuery \queryToken _ -> Hook.do
  state /\ _state <- Hook.useState initialState

  Hook.useQuery queryToken case _ of
    IsOn reply -> do
      { enabled } <- EH.get _state
      pure (Just (reply enabled))

  let
    label = if state.enabled then "On" else "Off"

    handleClick = Just do
      newState <- EH.modify _state \st -> st { enabled = not st.enabled }
      EH.raise (Toggled newState.enabled)

  Hook.pure do
    HH.button
      [ HP.title label
      , HE.onClick \_ -> handleClick
      ]
      [ HH.text label ]
