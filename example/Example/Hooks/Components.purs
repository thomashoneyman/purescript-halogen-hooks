module Example.Hooks.Components where

import Prelude

import Data.Maybe (maybe)
import Effect.Aff.Class (class MonadAff)
import Example.Hooks.UseWindowWidth (useWindowWidth)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hook as Hook

windowWidth :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
windowWidth = Hook.component \_ -> Hook.do
  width <- useWindowWidth
  Hook.pure $ HH.div_ [ HH.text $ "Current width: " <> maybe "" show width ]
