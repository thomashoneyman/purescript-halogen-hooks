module Test.Hooks.UseEffect.UseLifecycleEffect where

import Prelude

import Data.Array (fold, replicate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Halogen as H
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Spec (Spec, before, describe, it)
import Test.Setup.Eval (evalM, mkEval)
import Test.Setup.Log (initDriver, logShouldBe, readResult, writeLog)
import Test.Setup.Types (EffectType(..), Hook', HookM', HookType(..), LogRef, TestEvent(..), Log)

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
    eval = mkEval useLifecycleEffectLog

    hooksLog :: InterpretHookReason -> Log
    hooksLog reason = [ RunHooks reason, Render ]

  it "runs the effect on initialize" \ref -> do
    evalM ref $ eval $ H.tell H.Initialize
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      ]

  it "runs the effect on initialize and finalize" \ref -> do
    evalM ref $ (eval (H.tell H.Initialize) *> eval (H.tell H.Finalize))
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]

  it "doesn't run the effect other than initialize / finalize" \ref -> do
    evalM ref do
      eval $ H.tell H.Initialize
      { tick } <- liftAff $ readResult ref
      eval (H.tell $ H.Action tick) *> eval (H.tell $ H.Action tick)
      eval $ H.tell H.Finalize

    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      , fold $ replicate 2 $ fold [ pure ModifyState, hooksLog Step ]
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]
