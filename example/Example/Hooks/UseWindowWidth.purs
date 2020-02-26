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
import Halogen.EvalHookM as EH
import Halogen.Hook (Hook, UseEffect, UseState)
import Halogen.Hook as Hook
import Halogen.Query.EventSource as ES
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window

foreign import data UseWindowWidth :: Type -> Type

type UseWindowWidth' hooks = UseEffect (UseState (Maybe Int) hooks)

useWindowWidth :: forall q ps o m. MonadAff m => Hook q ps o m UseWindowWidth (Maybe Int)
useWindowWidth = Hook.coerce hook
  where
  hook :: Hook q ps o m UseWindowWidth' (Maybe Int)
  hook = Hook.do
    width /\ widthState <- Hook.useState Nothing

    Hook.useEffect [] do
      let
        readWidth = EH.put widthState <<< Just <=< liftEffect <<< Window.innerWidth

      window <- liftEffect HTML.window
      subscriptionId <- EH.subscribe do
        ES.eventListenerEventSource
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >>> map (fromEventTarget >>> readWidth))

      readWidth window
      pure (Just $ EH.unsubscribe subscriptionId)

    Hook.pure width

-- This function is missing from the purescript-web-html repository
fromEventTarget :: EventTarget -> HTML.Window
fromEventTarget = unsafeCoerce
