module Test.Integration.Issue73 where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Hooks (class HookNewtype, type (<>), Hook, UseEffect, UseMemo, UseRef, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver)
import Test.Setup.Log (getLogRef, logShouldBe, writeLog)
import Test.Setup.Types (EffectType(..), LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)

foreign import data UseImmediateRaiseAndReceive :: Hooks.HookType

type UseImmediateRaiseAndReceive' = UseState Int <> UseMemo Int <> UseEffect <> UseRef Int <> Hooks.Pure

instance HookNewtype UseImmediateRaiseAndReceive UseImmediateRaiseAndReceive'

interruptInitialize :: LogRef -> Hook Aff UseImmediateRaiseAndReceive Unit
interruptInitialize log = Hooks.wrap Hooks.do

  _ <- Hooks.useState 0
  _ <- Hooks.captures { once : true } Hooks.useMemo \_ -> 0

  Hooks.captures { once : true } Hooks.useTickEffect do
      writeLog (RunEffect (EffectBody 0)) log
      pure $ Just do
        writeLog (RunEffect (EffectCleanup 0)) log


  _ <- Hooks.useRef 0

  Hooks.pure unit

safeInitialize :: Spec Unit
safeInitialize = before ( initDriver interruptInitialize ) $ describe "safeInitialize" do

  it "initialization should be finished before Initialize arrives" \{ eval, ref } -> do
    evalM ref $ eval $ H.Initialize
    logShouldBe ref [ RunHooks Initialize, Render, RunEffect ( EffectBody 0 ) ]

  it "initialization should be finished before Receive arrives" \{ eval, ref } -> do
    logRef <- getLogRef ref
    evalM ref $ eval $ H.Receive logRef
    logShouldBe ref [ RunHooks Step, Render ]

  it "initialization should be finished before Finalize arrives" \{ eval, ref } -> do
    evalM ref $ eval $ H.Finalize
    logShouldBe ref [ RunHooks Finalize, Render ]

  it "initialization should be finished before Action arrives" \{ eval, ref } -> do
    evalM ref $ eval $ H.Action $ pure unit
    logShouldBe ref []
