module Test.Integration.Issue73 where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen (liftAff)
import Halogen as H
import Halogen.Hooks (class HookNewtype, type (<>), Hook, UseEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver, mkEval)
import Test.Setup.Log (getLogRef, logShouldBe, writeLog)
import Test.Setup.Types (EffectType(..), LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)

foreign import data UseImmediateRaiseAndReceive :: Hooks.HookType

type UseImmediateRaiseAndReceive' = UseEffect <> Hooks.Pure

instance HookNewtype UseImmediateRaiseAndReceive UseImmediateRaiseAndReceive'

interruptInitialize :: Aff Unit -> LogRef -> Hook Aff UseImmediateRaiseAndReceive Unit
interruptInitialize interrupt log = Hooks.wrap Hooks.do
  Hooks.captures { once : true } Hooks.useTickEffect do
      writeLog (RunEffect (EffectBody 0)) log
      liftAff interrupt
      pure $ Just do
        writeLog (RunEffect (EffectCleanup 0)) log

  Hooks.pure unit

safeInitialize :: Spec Unit
safeInitialize = before initDriver $ describe "safeInitialize" do

  let
    -- receive should simulate a parent component firing a Receive to the running hook in response to an action in 
    -- UseEffect
    receive ref = do
      logRef <- getLogRef ref
      evalM ref $ mkEval ( interruptInitialize $ pure unit ) ( H.Receive logRef )

  it "effect initialization should be safe from interuption by parent" \ref -> do
    
    evalM ref $ mkEval ( interruptInitialize $ receive ref ) H.Initialize

    logShouldBe ref initializeSteps

  where
  initializeSteps =
    [ RunHooks Initialize -- initialize hooks
    , Render -- first render occurs

    , RunEffect (EffectBody 0) -- run enqueued lifecycle effect's initializer
    , RunHooks Step -- get interrupted by parent
    , Render -- render because of parent
    ]
