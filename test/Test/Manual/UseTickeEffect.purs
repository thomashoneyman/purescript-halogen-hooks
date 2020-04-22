module Test.Manual.UseTickEffect where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (Hooked)
import Halogen.Hooks as Hooks

sproxy :: SProxy "useTickEffect"
sproxy = SProxy

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Unit o m
component = Hooks.component \_ -> hook

hook :: forall slots output m hooks. MonadEffect m
     => Hooked slots output m hooks (Hooks.UseEffect (Hooks.UseState Int hooks)) _
hook = Hooks.do
  state /\ tState <- Hooks.useState 0
  Hooks.captures { state } Hooks.useTickEffect do
    liftEffect $ log $
      "useTickEffect: This message appears in two situations. First, when the \
      \component is initialized. Second, every time the state value changes, \
      \but it appears only AFTER the cleanup message appears."
    pure $ Just do
      liftEffect $ log $
        "useTickEffect: [Cleanup Message]. This message appears in two \
        \situations. First, every time the state value changes. Second, \
        \when component is removed."

  Hooks.pure $
    HH.div_
      [ HH.p_
        [ HH.text $ "Click the button to change the value of the dependency, \
                    \which will trigger the useTickEffect code."
          ]
      , HH.button
        [ HE.onClick \_ -> Just $ Hooks.modify_ tState (_ + 1) ]
        [ HH.text $ "Trigger the `useTickEffect` code by \
                    \increasing the state value by 1"
        ]
      ]
