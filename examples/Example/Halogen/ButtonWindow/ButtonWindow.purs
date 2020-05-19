module Example.Halogen.Basic.ButtonWindow where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (Hook, HookM, UseEffect)
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Web.Event.Event (EventType(..), stopPropagation)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

newtype UseWindowClick hooks
  = UseWindowClick (UseEffect hooks)

derive instance newtypeUseWindowClick :: Newtype (UseWindowClick hooks) _

useWindowClick :: forall m. MonadAff m => HookM m Unit -> Hook m UseWindowClick Unit
useWindowClick handler =
  Hooks.wrap Hooks.do

    Hooks.useLifecycleEffect do
      window <- liftEffect HTML.window

      _ <- Hooks.subscribe do
        ES.eventListenerEventSource
          (EventType "click")
          (Window.toEventTarget window)
          (const $ Just handler)

      pure $ Just $ pure unit

    Hooks.pure unit

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  Hooks.component \_ _ -> Hooks.do
    -- Declare a new state variable, which we'll call "count"
    count /\ countId <- Hooks.useState 0

    -- Decrement count when clicked in window outside of button
    useWindowClick $ Hooks.modify_ countId (_ - 1)

    let
      evtToMaybeModify :: ME.MouseEvent -> Maybe (HookM m Unit)
      evtToMaybeModify evt =
        Just
          do
            -- stopPropagation is necessary so button click not also interpreted as window click.
            liftEffect $ stopPropagation $ ME.toEvent evt
            -- increment count
            Hooks.modify_ countId (_ + 1)

    Hooks.pure do
      HH.div_
        [ HH.p_ [ HH.text $ "You clicked on the button " <> show count <> " times. Try clicking outside of the button." ]
        , HH.button
            [ HE.onClick evtToMaybeModify ]
            [ HH.text "Click me" ]
        ]