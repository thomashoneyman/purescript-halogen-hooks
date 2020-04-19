module Test.UseMemo where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (Hooked)
import Halogen.Hooks as Hooks

sproxy :: SProxy "useMemo"
sproxy = SProxy

component :: forall q o m. H.Component HH.HTML q Unit o m
component = Hooks.component \_ -> hook

hook :: forall slots output m hooks
      . Hooked slots output m hooks (Hooks.UseMemo Int (Hooks.UseState Int (Hooks.UseState Int hooks))) _
hook = Hooks.do
  s1 /\ ts1 <- Hooks.useState 0
  s2 /\ ts2 <- Hooks.useState 0

  expensiveValue <- Hooks.captures { s1 } $ flip Hooks.useMemo \_ ->
    let _ = unsafePerformEffect (Console.log "useMemo: recalculating expensive value")
    in s1 + 5

  Hooks.pure $
    HH.div_
      [ HH.p_ [ HH.text $ "Expensive value is: " <> show expensiveValue ]
      , HH.button
        [ HE.onClick \_ -> Just $ Hooks.modify_ ts1 (_ + 1)
        ]
        [ HH.text $
            "Increase `s1` by 1, which will recompute the expensive value."
        ]
      , HH.br_
      , HH.button
        [ HE.onClick \_ -> Just $ Hooks.modify_ ts2 (_ + 1)
        ]
        [ HH.text $
            "Increase `s2` by 1, which will NOT recompute the expensive value."
        ]
      ]
