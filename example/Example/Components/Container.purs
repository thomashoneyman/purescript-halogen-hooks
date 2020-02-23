module Example.Components.Container (component) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Example.Components.Button as Button
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

data Action
  = HandleButton Button.Message
  | CheckButtonState

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

initialState :: State
initialState =
  { toggleCount: 0
  , buttonState: Nothing
  }

type ChildSlots =
  ( button :: Button.Slot Unit
  )

_button :: SProxy "button"
_button = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component = Hook.component \_ _ -> Hook.do
  state /\ _state <- Hook.useState initialState

  let
    handle :: Action -> Maybe (EH.EvalHookM ChildSlots o m Unit)
    handle = Just <<< case _ of
      HandleButton (Button.Toggled _) -> do
        EH.modify_ _state \st -> st { toggleCount = st.toggleCount + 1 }

      CheckButtonState -> do
        buttonState <- EH.query _button unit $ H.request Button.IsOn
        EH.modify_ _state _ { buttonState = buttonState }
        pure unit

  Hook.pure do
    HH.div_
      [ HH.slot _button unit Button.component unit (handle <<< HandleButton)
      , HH.p_
          [ HH.text $ "Button has been toggled " <> show state.toggleCount <> " time(s)" ]
      , HH.p_
          [ HH.text $ fold
              [ "Last time I checked, the button was: "
              , maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState
              , ". "
              ]
          , HH.button
              [ HE.onClick \_ -> handle CheckButtonState ]
              [ HH.text "Check now" ]
          ]
      ]
