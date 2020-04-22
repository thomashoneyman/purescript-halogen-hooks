module Test.Manual.Conditional where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (Component)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (Hook, UseEffect, UseState, useLifecycleEffect)
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

component :: forall query input output m. MonadAff m => Component HH.HTML query input output m
component = Hooks.component $ \_ -> Hooks.do
  count /\ countToken <- Hooks.useState 0
  state1 /\ stateToken1 <- Hooks.useState false
  useLifecycleEffect do
    Hooks.put stateToken1 true
    pure Nothing
  -- unique id to understand which hook execution is called by useEffect
  id <- Hooks.pure $ show $ unsafePerformEffect $ randomInt 0 10000
  Hooks.pure $ unsafePerformEffect $ liftEffect $ log $ "runConditional(" <> id <> "): " <> (show state1)
  state2 /\ stateToken2 <- Hooks.useState false
  useMyEffect stateToken2 id state2 { state1 }
  Hooks.pure do
    HH.div_
      [
        HH.text $ "count: " <> (show count)
      , HH.br_
      , HH.text $ "val: " <> (show state2)
      , HH.br_
      , HH.button [HE.onClick (\_ -> handleClick countToken count)] [HH.text "trigger count change" ]
      ]

  where
    useMyEffect stateToken2 id state2 deps@{ state1 : state1' } = Hooks.captures deps Hooks.useTickEffect do
      liftEffect $ log $ "CHANGE HANDLER TRIGGERED(" <> id <> "): " <> (show deps) <> ": " <> (show state2) <> ":" <> (show state1')
      Hooks.put stateToken2 state1'
      pure Nothing

    handleClick countToken count = Just do
      Hooks.put countToken (count + 1)
