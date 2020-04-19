module Test.UseState where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (Hooked, useState)
import Halogen.Hooks as Hooks

sproxy :: SProxy "useState"
sproxy = SProxy

component :: forall q o m. H.Component HH.HTML q Unit o m
component = Hooks.component \_ -> hook

hook :: forall slots output m hooks
      . Hooked slots output m hooks (Hooks.UseState Int hooks) _
hook = Hooks.do
  state /\ tState <- useState 0

  Hooks.pure $
    HH.div_
      [ HH.p_
        [ HH.text $ "Button value is: " <> show state ]
      , HH.button
        [ HE.onClick \_ -> Just $ Hooks.modify_ tState (_ + 1) ]
        [ HH.text "Increase the state value by 1" ]
      ]
