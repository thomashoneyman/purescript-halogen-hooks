module Example.Halogen.Basic.ButtonWindow where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.Hooks (Hook, HookM, UseEffect)
import Halogen.Query.EventSource as ES
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Window as Window

newtype UseWindowClick hooks
  = UseWindowClick (UseEffect hooks)

derive instance newtypeUseWindowClick :: Newtype (UseWindowClick hooks) _

useWindowClick :: forall m. MonadAff m => HookM m Unit -> Hook m UseWindowClick Unit
useWindowClick handler =
  Hooks.wrap Hooks.do

    Hooks.useLifecycleEffect do
      subscription <- subscribeToWindow
      pure $ Just $ Hooks.unsubscribe subscription

    Hooks.pure unit

  where

  subscribeToWindow :: HookM m H.SubscriptionId
  subscribeToWindow = do

    window <- liftEffect HTML.window

    let
      eventToMaybe :: Event.Event -> Maybe (HookM m Unit)
      eventToMaybe =
        const $ Just handler

    subscriptionId <-
      Hooks.subscribe do
        ES.eventListenerEventSource
          (EventType "click")
          (Window.toEventTarget window)
          eventToMaybe

    -- needed to unsubscribe
    pure subscriptionId

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  Hooks.component \_ _ -> Hooks.do
    -- Declare a new state variable, which we'll call "count"
    count /\ countId <- Hooks.useState 0

    let
      decCount :: HookM m Unit
      decCount = do
        Hooks.modify_ countId (_ - 2)

    useWindowClick decCount

    Hooks.pure do
      HH.div_
        [ HH.p_ [ HH.text $ "You clicked " <> show count <> "times" ]
        , HH.button
            -- How do I stop propagation here?
            [ HE.onClick \_ -> Just $ Hooks.modify_ countId (_ + 1) ]
            [ HH.text "Click me" ]
        ]
