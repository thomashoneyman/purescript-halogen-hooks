module Test.UseRef where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (HookM, Hooked)
import Halogen.Hooks as Hooks

sproxy :: SProxy "useRef"
sproxy = SProxy

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Unit o m
component = Hooks.component \_ -> hook

hook :: forall slots output m hooks props
      . MonadEffect m
     => Hooked slots output m hooks (Hooks.UseRef Int hooks) (HH.HTML props (HookM slots output m Unit))
hook = Hooks.do
  value /\ ref <- Hooks.useRef 0

  Hooks.pure $
    HH.div_
      [ HH.button
        [ HE.onClick \_ -> Just do
            val <- liftEffect $ Ref.read ref
            liftEffect $ log $ "useRef: current value is: " <> show val
        ]
        [ HH.text $ "Print the Ref's current value to the console."
        ]
      , HH.br_
      , HH.button
        [ HE.onClick \_ -> Just do
          liftEffect $ Ref.modify_ (_ + 1) ref
        ]
        [ HH.text $ "Increase the current value of the Ref by 1."
        ]
      ]
