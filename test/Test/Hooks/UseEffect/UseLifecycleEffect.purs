module Test.Hooks.UseEffect.UseLifecycleEffect where

import Prelude

import Data.Array (fold, replicate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Test.Eval (EvalSpec(..), evalM, mkEval)
import Test.Log (initDriver, logShouldBe, writeLog)
import Test.Spec (Spec, before, describe, it)
import Test.Types (EffectType(..), Hook', HookM', HookType(..), LogRef, TestEvent(..))

newtype LogHook h = LogHook (UseEffect (UseState Int h))

derive instance newtypeLogHook :: Newtype (LogHook h) _

useLifecycleEffectLog :: LogRef -> Hook' LogHook { tick :: HookM' Unit }
useLifecycleEffectLog log = Hooks.wrap Hooks.do
  -- used to force re-evaluation of the hook; this should not re-run the effect
  -- because lifecycle effects run only once.
  count /\ countState <- Hooks.useState 0

  Hooks.useLifecycleEffect do
    liftAff $ writeLog (RunEffect EffectBody) log
    pure $ Just do
      liftAff $ writeLog (RunEffect EffectCleanup) log

  Hooks.pure { tick: Hooks.modify_ countState (_ + 1) }

lifecycleEffectHook :: Spec Unit
lifecycleEffectHook = before initDriver $ describe "useLifecycleEffect" do
  let
    EvalSpec { initialize, handleAction, finalize } = mkEval useLifecycleEffectLog

    hooksLog reason =
      [ RunHooks reason, EvaluateHook UseStateHook, EvaluateHook UseEffectHook, Render ]

  it "runs the effect on initialize" \ref -> do
    _ <- evalM ref initialize
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      ]

  it "runs the effect on initialize and finalize" \ref -> do
    _ <- evalM ref (initialize *> finalize)
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]

  it "doesn't run the effect other than initialize / finalize" \ref -> do
    _ <- evalM ref do
      { tick } <- initialize
      handleAction tick *> handleAction tick
      finalize

    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      , fold $ replicate 2 $ fold [ pure ModifyState, hooksLog Step ]
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]
