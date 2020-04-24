module Test.Hooks.Multiple.Bug5 where

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

newtype RunTickAfterInitialEffectsHook h = LogHook (UseEffect (UseState Boolean (UseEffect (UseState Boolean (UseState Int h)))))

derive instance newtypeRunTickAfterInitialEffectsHook :: Newtype (RunTickAfterInitialEffectsHook h) _

rerunTickAfterInitialEffects :: LogRef -> Hook' RunTickAfterInitialEffectsHook { count :: Int, state1 :: Boolean, state2 :: Boolean }
rerunTickAfterInitialEffects log = Hooks.wrap Hooks.do
  count /\ countToken <- Hooks.useState 0
  state1 /\ stateToken1 <- Hooks.useState false
  Hooks.useLifecycleEffect do
    writeLog (RunEffect EffectBody) log
    Hooks.put stateToken1 true
    pure Nothing

  state2 /\ stateToken2 <- Hooks.useState false
  useMyEffect stateToken2 { state1 }
  Hooks.pure { count, state1, state2 }

  where
    useMyEffect stateToken2 deps@{ state1 : state1' } = Hooks.captures deps Hooks.useTickEffect do
      writeLog (RunEffect EffectBody) log
      Hooks.put stateToken2 state1'
      pure Nothing

rerunTickAfterInitialEffectsHook :: Spec Unit
rerunTickAfterInitialEffectsHook = before initDriver $ describe "rerunTickAfterInitialEffects" do
  let eval = mkEval rerunTickAfterInitialEffects

  it "effect runs on memo change and cleans up before next run" \ref -> do
    { count, state1, state2 } <- evalM ref do
      eval H.Initialize
      readResult ref

    count `shouldEqual` 0
    state1 `shouldEqual` true
    state2 `shouldEqual` true
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
  --
  -- it "effect is skipped when memos are unchanged" \ref -> do
  --   { count } <- evalM ref do
  --     eval H.Initialize
  --
  --     { toggle } <- readResult ref
  --     eval (H.Action toggle) *> eval (H.Action toggle)
  --
  --     eval H.Finalize
  --     readResult ref
  --
  --   -- Unlike the previous test, there should not be successive effect cleanup
  --   -- and evaluation during hook evaluations because deps are unchanged.
  --   logShouldBe ref $ fold
  --     [ initializeSteps
  --     , fold $ replicate 2 [ ModifyState, RunHooks Step, Render ]
  --     , finalizeSteps
  --     ]

  where
  initializeSteps =
    [ RunHooks Initialize      -- initialize hooks
    , RunEffect EffectBody     -- enqueued lifecycle effect occurs
    , RunEffect EffectBody     -- enqueued tick effect occurs
    , RunHooks Step            -- recheck state memos and find it has changed
    , RunEffect EffectCleanup  -- tick's finalizer runs
    , RunEffect EffectBody     -- tick's initializer runs
    , Render                   -- render again
    ]

  finalizeSteps =
    [ RunHooks Finalize, Render, RunEffect EffectCleanup ]
