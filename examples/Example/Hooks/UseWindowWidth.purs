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
import Halogen as H
import Halogen.Hooks (class HookEquals, class HookNewtype, type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Query.Event as HE
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Window as Window

foreign import data UseWindowWidth :: Hooks.HookType

type UseWindowWidth' = UseState (Maybe Int) <> UseEffect <> Hooks.Pure

instance newtypeUseWindowWidth :: HookEquals UseWindowWidth' h => HookNewtype UseWindowWidth h

useWindowWidth :: forall m. MonadAff m => Hook m UseWindowWidth (Maybe Int)
useWindowWidth = Hooks.wrap hook
  where
  hook :: Hook m UseWindowWidth' _
  hook = Hooks.do
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
        HE.eventListener
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >=> Window.fromEventTarget >>> map readWidth)

      readWidth window
      pure subscriptionId
