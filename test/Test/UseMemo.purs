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
      . Hooked slots output m hooks (Hooks.UseMemo Int (Hooks.UseMemo Int ((Hooks.UseMemo Int (Hooks.UseState Int (Hooks.UseState Int hooks)))))) _
hook = Hooks.do
  s1 /\ ts1 <- Hooks.useState 0
  s2 /\ ts2 <- Hooks.useState 0

  expensiveValue1 <- memoize1 { s1 }

  expensiveValue2 <- memoize2 { s2 }

  expensiveValue3 <- memoize3 { s1, s2 }

  Hooks.pure $
    HH.div_
      [ HH.ol_
        [ HH.li_ [ HH.text $ "Expensive value 1 is: " <> show expensiveValue1 ]
        , HH.li_ [ HH.text $ "Expensive value 2 is: " <> show expensiveValue2 ]
        , HH.li_ [ HH.text $ "Expensive value 3 is: " <> show expensiveValue3 ]
        ]
      , HH.button
        [ HE.onClick \_ -> Just $ Hooks.modify_ ts1 (_ + 1)
        ]
        [ HH.text $
            "Increase `s1` by 1, which will recompute 1 and 3."
        ]
      , HH.br_
      , HH.button
        [ HE.onClick \_ -> Just $ Hooks.modify_ ts2 (_ + 1)
        ]
        [ HH.text $
            "Increase `s2` by 1, which will recompute 2 and 3."
        ]
      ]
  where
    memoize1 deps@{s1} = Hooks.captures deps $ flip Hooks.useMemo \_ ->
      let _ = unsafePerformEffect (Console.log "useMemo: recalculating expensive value1")
      in s1 + 5

    memoize2 deps@{s2} = Hooks.captures deps $ flip Hooks.useMemo \_ ->
      let _ = unsafePerformEffect (Console.log "useMemo: recalculating expensive value2")
      in s2 + 5

    memoize3 deps@{s1, s2} = Hooks.captures deps $ flip Hooks.useMemo \_ ->
      let _ = unsafePerformEffect (Console.log "useMemo: recalculating expensive value3")
      in s1 + s2 + 5
