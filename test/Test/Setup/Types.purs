module Test.Setup.Types where

import Prelude

import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Aff.Driver.State (DriverState)
import Halogen.Hooks (HookM)
import Halogen.Hooks.Internal.Eval.Types (HookState, InterpretHookReason(..))
import Halogen.Hooks.Internal.Types (OutputValue, SlotType)

type HalogenF' q i m b a = H.HalogenF (HookState q i m b) (HookM m Unit) SlotType OutputValue m a

type DriverResultState r q a = DriverState r (HookState q LogRef Aff a) q (HookM Aff Unit) SlotType LogRef OutputValue

type LogRef = Ref Log

type Log = Array TestEvent

data TestEvent
  = RunHooks InterpretHookReason
  | ModifyState
  | RunEffect EffectType
  | RunMemo MemoType
  | EvaluateHook HookType
  | Render

derive instance Eq TestEvent

instance Show TestEvent where
  show = case _ of
    RunHooks reason ->
      append "RunHooks " case reason of
        Initialize -> "Initialize"
        Queued -> "Queued"
        Step -> "Step"
        Finalize -> "Finalize"

    ModifyState ->
      "ModifyState"

    RunEffect effect ->
      fold [ "RunEffect ", show effect ]

    RunMemo memo ->
      fold [ "RunMemo ", show memo ]

    EvaluateHook hook ->
      fold [ "EvaluateHook ", show hook ]

    Render ->
      "Render"

data HookType
  = UseStateHook
  | UseEffectHook
  | UseMemoHook
  | UseRefHook

derive instance Eq HookType
derive instance Generic HookType _

instance Show HookType where
  show = genericShow

data EffectType = EffectBody Int | EffectCleanup Int

derive instance Eq EffectType
derive instance Generic EffectType _

instance Show EffectType where
  show = genericShow

data MemoType = CalculateMemo Int

derive instance Eq MemoType
derive instance Generic MemoType _

instance Show MemoType where
  show = genericShow
