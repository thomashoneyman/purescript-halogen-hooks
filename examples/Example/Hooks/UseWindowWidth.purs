module Example.Hooks.UseWindowWidth
  ( useWindowWidth
  , UseWindowWidth
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.Hooks (Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window

type UseWindowWidth' hooks = UseEffect (UseState (Maybe Int) hooks)

foreign import data UseWindowWidth :: Type -> Type

useWindowWidth :: forall slots output m. MonadAff m => Hook slots output m UseWindowWidth (Maybe Int)
useWindowWidth = Hooks.publish hook
  where
  hook :: Hook slots output m UseWindowWidth' (Maybe Int)
  hook = Hooks.do
    width /\ widthState <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      let readWidth = Hooks.put widthState <<< Just <=< liftEffect <<< Window.innerWidth

      window <- liftEffect HTML.window
      subscriptionId <- Hooks.subscribe do
        ES.eventListenerEventSource
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >>> map (fromEventTarget >>> readWidth))

      readWidth window
      pure (Just $ Hooks.unsubscribe subscriptionId)

    Hooks.pure width

-- This function is missing from the purescript-web-html repository
fromEventTarget :: EventTarget -> HTML.Window
fromEventTarget = unsafeCoerce
