module Test.Hooks.UseEffect.UseLifecycleEffect where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
-- import Test.Eval (evalTestHookM, evalTestM, initDriver, runTestHook)
import Test.Spec (Spec, describe, pending)
import Test.Types (Hook', HookM')

data Log = EffectBody | EffectCleanup

derive instance eqLog :: Eq Log
derive instance genericLog :: Generic Log _

instance showLog :: Show Log where
  show = genericShow

newtype LogHook h = LogHook (UseEffect (UseState Int (UseState (Array Log) h)))

derive instance newtypeLogHook :: Newtype (LogHook h) _

useLifecycleEffectLog :: Hook' LogHook { tick :: HookM' Unit, logs :: Array Log }
useLifecycleEffectLog = Hooks.wrap Hooks.do
  -- used to accumulate logs from effect run
  log /\ logState <- Hooks.useState []

  -- used to force re-evaluation of the hook; this should not re-run the effect
  -- because lifecycle effects run only once
  count /\ countState <- Hooks.useState 0

  Hooks.useLifecycleEffect do
    Hooks.modify_ logState (_ `Array.snoc` EffectBody)
    pure $ Just do
      Hooks.modify_ logState (_ `Array.snoc` EffectCleanup)

  Hooks.pure { tick: Hooks.modify_ countState (_ + 1), logs: log }

lifecycleEffectHook :: Spec Unit
lifecycleEffectHook = describe "useLifecycleEffect" do
  pending "implement"

{-
  it "runs the effect on initialize" do
    ref <- initDriver

    Tuple _ events <- evalTestM ref $ runWriterT do
      runTestHook Initialize useLifecycleEffectLog

    -- TODO: this _should_ be firing modifyState events because of the effect
    events `shouldEqual`
      [ RunHooks Initialize -- state 1
      , RunHooks Initialize -- state 2
      , RunHooks Initialize -- effect
      , Render
      ]

    -- Necessary for now, as this returns the _old_ state; logs will not include
    -- the finalizer step.
    Tuple { logs } _ <- evalTestM ref $ runWriterT do
      runTestHook Queued useLifecycleEffectLog

    logs `shouldEqual` [ EffectBody ]

  it "runs the effect on initialize and finalize" do
    ref <- initDriver

    _ <- evalTestM ref $ runWriterT do
      runTestHook Initialize useLifecycleEffectLog

    Tuple _ events <- evalTestM ref $ runWriterT do
      runTestHook Finalize useLifecycleEffectLog

    events `shouldEqual`
      [ RunHooks Finalize -- state 1
      , RunHooks Finalize -- state 2
      , RunHooks Finalize -- effect
      ]

    -- Necessary for now, as this returns the _old_ state; logs will not include
    -- this second finalizer step.
    Tuple { logs } _ <- evalTestM ref $ runWriterT do
      runTestHook Queued useLifecycleEffectLog

    logs `shouldEqual` [ EffectBody, EffectCleanup ]

  it "doesn't run the effect other than initialize / finalize" do
    ref <- initDriver

    Tuple { tick } _ <- evalTestM ref $ runWriterT do
      runTestHook Initialize useLifecycleEffectLog

    Tuple _ events <- evalTestM ref $ runWriterT do
      let runHooks = void $ runTestHook Step useLifecycleEffectLog

      -- ticks should cause hooks to run, but shouldn't cause the effect itself
      -- to evaluate again
      evalTestHookM runHooks tick
        *> evalTestHookM runHooks tick
        *> evalTestHookM runHooks tick

    shouldEqual events $ Array.concat $ Array.replicate 3
      [ ModifyState
      , RunHooks Step
      , RunHooks Step
      , RunHooks Step
      , Render
      ]

    _ <- evalTestM ref $ runWriterT do
      runTestHook Finalize useLifecycleEffectLog

    -- Necessary for now; returns the _old_ state, so this finalizer isn't
    -- counted in the return
    Tuple { logs } _ <- evalTestM ref $ runWriterT do
      runTestHook Queued useLifecycleEffectLog

    -- Despite the hooks running multiple times, the effect should have only
    -- run once
    logs `shouldEqual` [ EffectBody, EffectCleanup ]

-}
