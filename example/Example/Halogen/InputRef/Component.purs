module Example.Halogen.InputRef.Component where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.EvalHookM (EvalHookM)
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hook as Hook
import Web.HTML.HTMLElement (focus)

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  let
    refLabel = H.RefLabel "inputElement"

    handleButtonClick :: EvalHookM _ o m Unit
    handleButtonClick = do
      EH.getHTMLElementRef refLabel >>= traverse_ (focus >>> liftEffect)

  Hook.pure do
    HH.div
      [ ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.ref refLabel
          ]
      , HH.button
          [ HE.onClick \_ -> Just handleButtonClick ]
          [ HH.text "Focus the input" ]
      ]
