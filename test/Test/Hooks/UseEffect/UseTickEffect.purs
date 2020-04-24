module Test.Hooks.UseEffect.UseTickEffect where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver, mkEval)
import Test.Setup.Log (logShouldBe, readResult, writeLog)
import Test.Setup.Types (EffectType(..), Hook', HookM', LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype LogHook h = LogHook (UseEffect (UseState Boolean (UseState Int h)))

derive instance newtypeLogHook :: Newtype (LogHook h) _

useTickEffectLog :: LogRef -> Hook' LogHook { increment :: HookM' Unit, toggle :: HookM' Unit, count :: Int }
useTickEffectLog log = Hooks.wrap Hooks.do
  count /\ countState <- Hooks.useState 0
  toggle /\ toggleState <- Hooks.useState false
  useLogger { count }
  Hooks.pure
    { count
    , increment: Hooks.modify_ countState (_ + 1)
    , toggle: Hooks.modify_ toggleState not
    }
  where
  useLogger deps@{ count } = Hooks.captures deps Hooks.useTickEffect do
    writeLog (RunEffect EffectBody) log
    pure $ Just do
      writeLog (RunEffect EffectCleanup) log

tickEffectHook :: Spec Unit
tickEffectHook = before initDriver $ describe "useTickEffect" do
  let eval = mkEval useTickEffectLog

  it "effect runs on initialize and cleans up on finalize" \ref -> do
    evalM ref $ eval H.Initialize *> eval H.Finalize
    logShouldBe ref $ fold [ initializeSteps, finalizeSteps ]

  it "effect runs on memo change and cleans up before next run" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize

      { increment } <- readResult ref
      eval (H.Action increment) *> eval (H.Action increment)

      eval H.Finalize
      readResult ref

    count `shouldEqual` 2
    logShouldBe ref $ fold
      [ initializeSteps
      , fold $ replicate 2
          [ ModifyState
          , RunHooks Step
          , Render
          , RunEffect EffectCleanup
          , RunEffect EffectBody
          ]
      , finalizeSteps
      ]

  it "effect is skipped when memos are unchanged" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize

      { toggle } <- readResult ref
      eval (H.Action toggle) *> eval (H.Action toggle)

      eval H.Finalize
      readResult ref

    -- Unlike the previous test, there should not be successive effect cleanup
    -- and evaluation during hook evaluations because deps are unchanged.
    logShouldBe ref $ fold
      [ initializeSteps
      , fold $ replicate 2 [ ModifyState, RunHooks Step, Render ]
      , finalizeSteps
      ]

  where
  initializeSteps =
    [ RunHooks Initialize, Render, RunEffect EffectBody ]

  finalizeSteps =
    [ RunHooks Finalize, Render, RunEffect EffectCleanup ]