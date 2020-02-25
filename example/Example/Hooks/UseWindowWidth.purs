module Example.Hooks.UseWindowWidth
  ( useWindowWidth
  , UseWindowWidth
  )
  where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.Hook (Hook, UseFinalizer, UseInitializer, UseState)
import Halogen.Hook as Hook
import Halogen.Query.EventSource as ES
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window

foreign import data UseWindowWidth :: Type -> Type

type UseWindowWidth' hooks =
  UseFinalizer (UseInitializer (UseState (Maybe H.SubscriptionId) (UseState (Maybe Int) hooks)))

useWindowWidth :: forall q ps o m. MonadAff m => Hook q ps o m UseWindowWidth (Maybe Int)
useWindowWidth = Hook.coerce hook
  where
  hook :: Hook q ps o m UseWindowWidth' (Maybe Int)
  hook = Hook.do
    width /\ widthState <- Hook.useState Nothing
    subscriptionId /\ subscriptionIdState <- Hook.useState Nothing

    Hook.useInitializer do
      window <- liftEffect HTML.window
      let readWidth = EH.put widthState <<< Just <=< liftEffect <<< Window.innerWidth
      readWidth window
      sid <- EH.subscribe do
        ES.eventListenerEventSource
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >>> map (fromEventTarget >>> readWidth))
      EH.put subscriptionIdState (Just sid)

    Hook.useFinalizer do
      for_ subscriptionId EH.unsubscribe

    Hook.pure width

-- This function is missing from the purescript-web-html repository
fromEventTarget :: EventTarget -> HTML.Window
fromEventTarget = unsafeCoerce
