module Test.UseLifecycleEffect where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (Hooked, useLifecycleEffect)
import Halogen.Hooks as Hooks

sproxy :: SProxy "useLifecycleEffect"
sproxy = SProxy

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Unit o m
component = Hooks.component \_ -> hook

hook :: forall slots output m hooks. MonadEffect m
     => Hooked slots output m hooks (Hooks.UseEffect hooks) _
hook = Hooks.do
  useLifecycleEffect do
    liftEffect $ log $
      "useLifecycleEffect: this message appears when component is initialized."
    pure $ Just do
      liftEffect $ log $
        "useLifecycleEffect: this message appears when component is removed."

  Hooks.pure $
    HH.div_
      [ HH.text $ "Lifecycle effect component. Check the console."
      ]
