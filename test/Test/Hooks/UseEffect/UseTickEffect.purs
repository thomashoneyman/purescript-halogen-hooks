module Test.Hooks.UseEffect.UseTickEffect where

import Prelude

import Data.Array (replicate)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Halogen as H
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, mkEval)
import Test.Setup.Log (initDriver, logShouldBe, readResult, writeLog)
import Test.Setup.Types (EffectType(..), Hook', HookM', HookType(..), LogRef, TestEvent(..))
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
    liftAff $ writeLog (RunEffect EffectBody) log
    pure $ Just do
      liftAff $ writeLog (RunEffect EffectCleanup) log

tickEffectHook :: Spec Unit
tickEffectHook = before initDriver $ describe "useTickEffect" do
  let
    eval = mkEval useTickEffectLog
    hooksLog reason =
      [ RunHooks reason, EvaluateHook UseStateHook, EvaluateHook UseStateHook, EvaluateHook UseEffectHook, Render ]

  it "effect runs on initialize and cleans up on finalize" \ref -> do
    evalM ref (eval (H.tell H.Initialize) *> eval (H.tell H.Finalize))
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]

  it "effect runs on memo change and cleans up before next run" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      { increment } <- liftAff $ readResult ref
      eval (H.tell $ H.Action increment) *> eval (H.tell $ H.Action increment)
      eval $ H.tell H.Finalize
      liftAff $ readResult ref

    count `shouldEqual` 2
    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      , fold $ replicate 2 $ fold
          [ pure ModifyState
          , hooksLog Step
          , pure (RunEffect EffectCleanup)
          , pure (RunEffect EffectBody)
          ]
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]

  it "effect is skipped when memos are unchanged" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      { toggle } <- liftAff $ readResult ref
      eval (H.tell $ H.Action toggle) *> eval (H.tell $ H.Action toggle)
      eval $ H.tell H.Finalize
      liftAff $ readResult ref

    logShouldBe ref $ fold
      [ hooksLog Initialize
      , pure (RunEffect EffectBody)
      -- Unlike the previous test, there should not be successive effect cleanup
      -- and evaluation during these evaluations because deps are unchanged
      , fold $ replicate 2 $ fold
          [ pure ModifyState
          , hooksLog Step
          ]
      , hooksLog Finalize
      , pure (RunEffect EffectCleanup)
      ]
