module Test.Hooks.Primitive.UseEffect.UseLifecycleEffect where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Hooks (Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver, mkEval)
import Test.Setup.Log (logShouldBe, readResult, writeLog)
import Test.Setup.Types (EffectType(..), LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)

newtype LogHook h = LogHook (UseEffect (UseState Int h))

derive instance newtypeLogHook :: Newtype (LogHook h) _

useLifecycleEffectLog :: LogRef -> Hook Aff LogHook { tick :: HookM Aff Unit }
useLifecycleEffectLog log = Hooks.wrap Hooks.do
  -- used to force re-evaluation of the hook; this should not re-run the effect
  -- because lifecycle effects run only once.
  state /\ modifyState <- Hooks.useState 0

  Hooks.useLifecycleEffect do
    writeLog (RunEffect (EffectBody 0)) log
    pure $ Just do
      writeLog (RunEffect (EffectCleanup 0)) log

  Hooks.pure { tick: modifyState (_ + 1) }

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
