module Test.Hooks.UseEffect.UseLifecycleEffect where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Component (InterpretHookReason(..), evalHookM, runWithQueue)
import Test.Eval (evalM, flushLog, initDriver, interpretUseHookFn, readLog)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (EffectType(..), Hook', HookM', HookType(..), LogRef, TestEvent(..))

newtype LogHook h = LogHook (UseEffect (UseState Int h))

derive instance newtypeLogHook :: Newtype (LogHook h) _

useLifecycleEffectLog :: LogRef -> Hook' LogHook { tick :: HookM' Unit }
useLifecycleEffectLog log = Hooks.wrap Hooks.do
  -- used to force re-evaluation of the hook; this should not re-run the effect
  -- because lifecycle effects run only once
  count /\ countState <- Hooks.useState 0

  Hooks.useLifecycleEffect do
    liftEffect $ Ref.modify_ (_ `Array.snoc` RunEffect EffectBody) log
    pure $ Just do
      liftEffect $ Ref.modify_ (_ `Array.snoc` RunEffect EffectCleanup) log

  Hooks.pure { tick: Hooks.modify_ countState (_ + 1) }

lifecycleEffectHook :: Spec Unit
lifecycleEffectHook = describe "useLifecycleEffect" do
  it "runs the effect on initialize" do
    ref <- initDriver

    _ <- evalM ref do
      runWithQueue $ interpretUseHookFn Initialize useLifecycleEffectLog

    log0 <- readLog ref

    -- TODO: this _should_ be firing modifyState events because of the effect
    log0 `shouldEqual`
      [ RunHooks
      , EvaluateHook Initialize UseStateHook
      , EvaluateHook Initialize UseEffectHook
      , Render
      ]

    -- Necessary for now, as this returns the _old_ state; logs will not include
    -- the finalizer step.
    _ <- evalM ref do
      runWithQueue $ interpretUseHookFn Queued useLifecycleEffectLog

    log1 <- readLog ref

    log1 `shouldEqual` [ RunEffect EffectBody, RunEffect EffectCleanup ]

  it "runs the effect on initialize and finalize" do
    ref <- initDriver

    _ <- evalM ref do
      runWithQueue $ interpretUseHookFn Initialize useLifecycleEffectLog

    flushLog ref

    _ <- evalM ref do
      runWithQueue $ interpretUseHookFn Finalize useLifecycleEffectLog

    log <- readLog ref

    log `shouldEqual`
      [ RunHooks
      , EvaluateHook Finalize UseStateHook
      , EvaluateHook Finalize UseEffectHook
      ]

  it "doesn't run the effect other than initialize / finalize" do
    ref <- initDriver

    { tick } <- evalM ref do
      runWithQueue $ interpretUseHookFn Initialize useLifecycleEffectLog

    flushLog ref

    _ <- evalM ref do
      let runHooks = interpretUseHookFn Step useLifecycleEffectLog

      -- ticks should cause hooks to run, but shouldn't cause the effect itself
      -- to evaluate again
      evalHookM runHooks tick
        *> evalHookM runHooks tick
        *> evalHookM runHooks tick

      runWithQueue $ interpretUseHookFn Finalize useLifecycleEffectLog

    log <- readLog ref

    shouldEqual log $ Array.concat $ Array.replicate 3
      [ ModifyState
      , RunHooks
      , EvaluateHook Step UseStateHook
      , EvaluateHook Step UseEffectHook
      , Render
      ]
