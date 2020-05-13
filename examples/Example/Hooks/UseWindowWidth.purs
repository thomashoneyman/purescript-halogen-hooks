module Example.Hooks.UseWindowWidth
  ( useWindowWidth
  , UseWindowWidth
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Hooks (Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window

newtype UseWindowWidth hooks = UseWindowWidth (UseEffect (UseState (Maybe Int) hooks))

derive instance newtypeUseWindowWidth :: Newtype (UseWindowWidth hooks) _

useWindowWidth :: forall m. MonadAff m => Hook m UseWindowWidth (Maybe Int)
useWindowWidth = Hooks.wrap Hooks.do
  width /\ widthId <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    subscription <- subscribeToWindow (Hooks.put widthId)
    pure $ Just $ Hooks.unsubscribe subscription

  Hooks.pure width
  where
  subscribeToWindow :: (Maybe Int -> HookM m Unit) -> HookM m H.SubscriptionId
  subscribeToWindow setWidth = do
    let readWidth = setWidth <<< Just <=< liftEffect <<< Window.innerWidth

    window <- liftEffect HTML.window
    subscriptionId <- Hooks.subscribe do
      ES.eventListenerEventSource
        (EventType "resize")
        (Window.toEventTarget window)
        (Event.target >>> map (fromEventTarget >>> readWidth))

    readWidth window
    pure subscriptionId

-- This function is missing from the purescript-web-html repository
fromEventTarget :: EventTarget -> HTML.Window
fromEventTarget = unsafeCoerce
