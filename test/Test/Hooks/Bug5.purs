module Test.Hooks.Bug5 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Hooks (Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver, mkEval)
import Test.Setup.Log (logShouldBe, readResult, writeLog)
import Test.Setup.Types (EffectType(..), LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (fail)

newtype RunTickAfterInitialEffectsHook h = LogHook (UseEffect (UseState Int (UseEffect (UseState Int (UseState Int h)))))

derive instance newtypeRunTickAfterInitialEffectsHook :: Newtype (RunTickAfterInitialEffectsHook h) _

rerunTickAfterInitialEffects :: LogRef -> Hook Aff RunTickAfterInitialEffectsHook { count :: Int, state1 :: Int, state2 :: Int }
rerunTickAfterInitialEffects log = Hooks.wrap Hooks.do
  count /\ countToken <- Hooks.useState 0
  state1 /\ stateToken1 <- Hooks.useState 1
  Hooks.useLifecycleEffect do
    writeLog (RunEffect (EffectBody 0)) log
    Hooks.modify_ stateToken1 (_ + 1)
    pure $ Just do
      writeLog (RunEffect (EffectCleanup 0)) log

  state2 /\ stateToken2 <- Hooks.useState 0
  useMyEffect stateToken2 { state1 }
  Hooks.pure { count, state1, state2 }

  where
    useMyEffect stateToken2 deps@{ state1 : state1' } = Hooks.captures deps Hooks.useTickEffect do
      writeLog (RunEffect (EffectBody 1)) log
      Hooks.modify_ stateToken2 (_ + state1')
      pure $ Just do
        writeLog (RunEffect (EffectCleanup 1)) log

rerunTickAfterInitialEffectsHook :: Spec Unit
rerunTickAfterInitialEffectsHook = before initDriver $ describe "rerunTickAfterInitialEffects" do
  let eval = mkEval rerunTickAfterInitialEffects

  it "tick effect reruns when memos are updated via initial effect's state modification" \ref -> do
    { count, state1, state2 } <- evalM ref do
      eval H.Initialize
      readResult ref

    when (count /= 0) $ fail $ "count /= 0. count: " <> show count
    when (state1 /= 2) $ fail $ "state1 /= 2. state1: " <> show state1
    when (state2 /= 3) $ fail $ "state2 /= 3. state2: " <> show state2
    logShouldBe ref initializeSteps

  where
  initializeSteps =
    [ RunHooks Initialize         -- initialize hooks
    , Render                      -- first render occurs

    , RunEffect (EffectBody 0)    -- run enqueued lifecycle effect's initializer
    , ModifyState                 -- state1 gets incremented to 2,
                                  --    which should cause tick effect to rerun
    , RunHooks Queued             -- rerun all non-effect hooks to update state
                                  --    now the returned `state1` value is 2
    , Render                      -- render

    , RunEffect (EffectBody 1)    -- run enqueued tick effect's initializer
    , ModifyState                 -- state2 gets incremented to 1
                                  -- (i.e. 0 + state1's initial value: 1)
    , RunHooks Queued             -- rerun all non-effect hooks to update state
                                  --    now the returned `state2` value is 1
    , Render                      -- render

    , RunHooks Step               -- rerun hooks in case tick effect updated state
    , Render


    , RunEffect (EffectCleanup 1) -- tick effect is rerun due to lifecycle effect's
                                  -- initializer modifying its dependencies
    , RunEffect (EffectBody 1)
    , ModifyState                 -- state2 gets incremented to 3
                                  -- (i.e. 1 + state2's current value: 2)

    , RunHooks Queued             -- rerun all non-effect hooks to update state
                                  --    now the returned `state2` value is 1
    , Render                      -- render


    , RunHooks Step               -- rerun hooks in case tick effect updated state
    , Render
    ]
