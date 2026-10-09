module Example.Halogen.InputRef.Component where

import Prelude

import Data.Foldable (traverse_)
import Effect.Class (class MonadEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Web.HTML.HTMLElement (focus)

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  let
    refLabel :: H.RefLabel
    refLabel = H.RefLabel "inputElement"

    handleButtonClick :: HookM m Unit
    handleButtonClick = do
      Hooks.getHTMLElementRef refLabel >>= traverse_ (focus >>> liftEffect)

  Hooks.pure do
    HH.div_
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.ref refLabel
          ]
      , HH.button
          [ HE.onClick \_ -> handleButtonClick ]
          [ HH.text "Focus the input" ]
      ]
