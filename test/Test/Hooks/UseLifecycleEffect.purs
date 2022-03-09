module Test.Hooks.UseLifecycleEffect where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Hooks (type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver, mkEval)
import Test.Setup.Log (logShouldBe, readResult, writeLog)
import Test.Setup.Types (EffectType(..), LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)

type Interface = { tick :: HookM Aff Unit }

type UseLogHook = UseState Int <> UseEffect <> Hooks.Pure

useLifecycleEffectLog :: LogRef -> Hook Aff UseLogHook Interface
useLifecycleEffectLog log = Hooks.do
  -- used to force re-evaluation of the hook; this should not re-run the effect
  -- because lifecycle effects run only once.
  _ /\ stateId <- Hooks.useState 0

  Hooks.useLifecycleEffect do
    writeLog (RunEffect (EffectBody 0)) log
    pure $ Just do
      writeLog (RunEffect (EffectCleanup 0)) log

  Hooks.pure { tick: Hooks.modify_ stateId (_ + 1) }

lifecycleEffectHook :: Spec Unit
lifecycleEffectHook = before initDriver $ describe "useLifecycleEffect" do
  let eval = mkEval useLifecycleEffectLog

  it "runs the effect on initialize" \ref -> do
    evalM ref $ eval H.Initialize
    logShouldBe ref initializeSteps

  it "runs the effect on initialize and finalize" \ref -> do
    evalM ref $ eval H.Initialize *> eval H.Finalize
    logShouldBe ref $ fold [ initializeSteps, finalizeSteps ]

  it "doesn't run the effect other than initialize / finalize" \ref -> do
    evalM ref do
      eval H.Initialize

      { tick } <- readResult ref
      eval (H.Action tick) *> eval (H.Action tick)

      eval H.Finalize

    logShouldBe ref $ fold
      [ initializeSteps
      , fold $ replicate 2 [ ModifyState, RunHooks Step, Render ]
      , finalizeSteps
      ]

  where
  initializeSteps =
    [ RunHooks Initialize, Render, RunEffect (EffectBody 0) ]

  finalizeSteps =
    [ RunHooks Finalize, Render, RunEffect (EffectCleanup 0) ]
