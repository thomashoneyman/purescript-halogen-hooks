module Test.Setup.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Aff.Driver.State (DriverState)
import Halogen.HTML as HH
import Halogen.Hooks (HookM)
import Halogen.Hooks.Internal.Eval.Types (State, InterpretHookReason)
import Halogen.Hooks.Internal.Types (OutputValue, SlotType)

type HalogenF' q i m b a = H.HalogenF (State q i m b) (HookM m Unit) SlotType OutputValue m a

type DriverResultState r q a = DriverState HH.HTML r (State q LogRef Aff a) q (HookM Aff Unit) SlotType LogRef OutputValue

type LogRef = Ref Log

type Log = Array TestEvent

data TestEvent
  = RunHooks InterpretHookReason
  | ModifyState
  | RunEffect EffectType
  | RunMemo MemoType
  | EvaluateHook HookType
  | Render

derive instance eqTestEvent :: Eq TestEvent
derive instance genericTestEvent :: Generic TestEvent _

instance showTestEvent :: Show TestEvent where
  show = genericShow

data HookType
  = UseStateHook
  | UseEffectHook
  | UseMemoHook
  | UseRefHook

derive instance eqHookType :: Eq HookType
derive instance genericHookType :: Generic HookType _

instance showHookType :: Show HookType where
  show = genericShow

data EffectType = EffectBody Int | EffectCleanup Int

derive instance eqEffectType :: Eq EffectType
derive instance genericEffectType :: Generic EffectType _

instance showEffectType :: Show EffectType where
  show = genericShow

data MemoType = CalculateMemo Int

derive instance eqMemoType :: Eq MemoType
derive instance genericMemoType :: Generic MemoType _

instance showMemoType :: Show MemoType where
  show = genericShow
