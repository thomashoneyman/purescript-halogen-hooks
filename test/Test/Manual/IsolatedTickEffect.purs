module Test.Manual.IsolatedTickEffect where

import Prelude
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

newtype UseConditional hooks = UseConditional (UseEffect (UseState (Boolean) hooks))
derive instance newtypeUseConditional :: Newtype (UseConditional hooks) _

useConditional :: forall slots output m . MonadAff m => Boolean -> Hook slots output m UseConditional (Boolean)
useConditional opt = Hooks.wrap Hooks.do
  -- unique id to understand which hook execution is called by useEffect
  id <- Hooks.pure $ show $ unsafePerformEffect $ randomInt 0 10000
  Hooks.pure $ unsafePerformEffect $ liftEffect $ log $ "runConditional(" <> id <> "): " <> (show opt)
  state /\ stateToken <- Hooks.useState false
  useMyEffect stateToken id state { opt }
  Hooks.pure state
  where
    useMyEffect stateToken id state deps@{ opt : opt' } = Hooks.captures deps Hooks.useTickEffect do
      liftEffect $ log $ "CHANGE HANDLER TRIGGERED(" <> id <> "): " <> (show deps) <> ": " <> (show state) <> ":" <> (show opt')
      Hooks.put stateToken opt'
      pure Nothing

newtype UseConditionalTest hooks = UseConditionalTest (UseConditional (UseEffect (UseState (Boolean) hooks)))
derive instance newtypeUseConditionalTest :: Newtype (UseConditionalTest hooks) _

useConditionalTest :: forall slots output m . MonadAff m => Hook slots output m UseConditionalTest (Boolean)
useConditionalTest = Hooks.wrap Hooks.do
  state /\ stateToken <- Hooks.useState false
  useLifecycleEffect do
    Hooks.put stateToken true
    pure Nothing
  val <- useConditional state
  Hooks.pure val

component :: forall query input output m. MonadAff m => Component HH.HTML query input output m
component = Hooks.component $ \_ -> Hooks.do
  count /\ countToken <- Hooks.useState 0
  val <- useConditionalTest
  Hooks.pure do
    HH.div_
      [
        HH.text $ "count: " <> (show count)
      , HH.br_
      , HH.text $ "val: " <> (show val)
      , HH.br_
      , HH.button [HE.onClick (\_ -> handleClick countToken count)] [HH.text "trigger count change" ]
      ]

  where
    handleClick countToken count = Just do
      Hooks.put countToken (count + 1)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body
