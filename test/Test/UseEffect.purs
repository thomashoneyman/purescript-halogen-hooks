module Test.UseEffect where

import Prelude

import Control.Monad.Writer (runWriterT)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceM)
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..))
import Test.Eval (evalTestHook, evalTestHookM, evalTestM, initDriver)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (Hook', TestEvent(..), HookM')

effectHook :: Spec Unit
effectHook = describe "useEffect" do
  pending "effect body runs on state change"
  pending "effect cleanup runs on state change"
  pending "effect is run when memos change"
  pending "effect is skipped when memos are unchanged"

  lifecycleEffectHook
  -- tickEffectHook

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
    traceM "running effect body"
    pure $ Just $ Hooks.modify_ logState (_ `Array.snoc` EffectCleanup)

  Hooks.pure { tick: Hooks.modify_ countState (_ + 1), logs: log }

lifecycleEffectHook :: Spec Unit
lifecycleEffectHook = describe "useLifecycleEffect" do
  it "runs the effect on initialize" do
    ref <- initDriver

    Tuple { logs } events <- evalTestM ref $ runWriterT do
      evalTestHook Initialize useLifecycleEffectLog

    events `shouldEqual`
      [ RunHooks Initialize -- state 1
      , RunHooks Initialize -- state 2
      , RunHooks Initialize -- effect
      , Render
      ]

    logs `shouldEqual` [ EffectBody ]

  it "runs the effect on finalize" do
    ref <- initDriver

    _ <- evalTestM ref $ runWriterT do
      evalTestHook Initialize useLifecycleEffectLog

    Tuple { logs } events <- evalTestM ref $ runWriterT do
      evalTestHook Finalize useLifecycleEffectLog

    events `shouldEqual`
      [ RunHooks Finalize -- state 1
      , RunHooks Finalize -- state 2
      , RunHooks Finalize -- effect
      ]

    logs `shouldEqual` [ EffectCleanup ]

  it "doesn't run the effect other than initialize / finalize" do
    ref <- initDriver

    Tuple { tick } _ <- evalTestM ref $ runWriterT do
      evalTestHook Initialize useLifecycleEffectLog

    Tuple _ events <- evalTestM ref $ runWriterT do
      let runHooks = void $ evalTestHook Step useLifecycleEffectLog

      -- ticks should cause hooks to run, but shouldn't cause the effect itself
      -- to evaluate again
      evalTestHookM runHooks tick *> evalTestHookM runHooks tick

    events `shouldEqual`
      [ ModifyState
      , RunHooks Step
      , RunHooks Step
      , RunHooks Step
      , Render

      , ModifyState
      , RunHooks Step
      , RunHooks Step
      , RunHooks Step
      , Render
      ]

    Tuple { logs } _ <- evalTestM ref $ runWriterT do
      evalTestHook Finalize useLifecycleEffectLog

    -- Despite the hooks running multiple times, the effect should have only
    -- run once
    logs `shouldEqual` [ EffectBody, EffectCleanup ]
