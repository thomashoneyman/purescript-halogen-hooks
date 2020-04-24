module Test.Setup.Types where

import Prelude

import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Aff.Driver.State (DriverState)
import Halogen.HTML as HH
import Halogen.Hooks (Hook, HookF, HookM, Hooked)
import Halogen.Hooks.Internal.Eval.Types (HookState, InterpretHookReason)
import Halogen.Hooks.Internal.UseHookF (UseHookF)

type HookState' a = HookState (Const Void) LogRef () Void Aff a

type DriverResultState r q a = DriverState HH.HTML r (HookState' a) q (HookM' Unit) () LogRef Void

type Hooked' hookType a = Hooked () Void Aff Unit hookType a

type Hook' hookType a = Hook () Void Aff hookType a

type UseHookF' a = UseHookF () Void Aff a

type HookF' a = HookF () Void Aff a

type HookM' a = HookM () Void Aff a

type HalogenQ' a = H.HalogenQ (Const Void) (HookM' Unit) LogRef a

type HalogenM' b a = H.HalogenM (HookState' b) (HookM' Unit) () Void Aff a

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

data EffectType = EffectBody | EffectCleanup

derive instance eqEffectType :: Eq EffectType
derive instance genericEffectType :: Generic EffectType _

instance showEffectType :: Show EffectType where
  show = genericShow

data MemoType = CalculateMemo Int

derive instance eqMemoType :: Eq MemoType
derive instance genericMemoType :: Generic MemoType _

instance showMemoType :: Show MemoType where
  show = genericShow
