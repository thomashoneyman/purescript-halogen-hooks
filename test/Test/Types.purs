module Test.Types where

import Prelude

import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Aff.Driver.State (DriverState)
import Halogen.HTML as HH
import Halogen.Hooks (Hook, HookF, HookM, UseHookF)
import Halogen.Hooks.Component (HookState, InterpretHookReason)

type HookState' a = HookState (Const Void) LogRef () Void Aff a

type DriverResultState r a = DriverState HH.HTML r (HookState' a) (Const Void) (HookM' Unit) () LogRef Void

type Hook' hookType a = Hook () Void Aff hookType a

type UseHookF' a = UseHookF () Void Aff a

type HookF' a = HookF () Void Aff a

type HookM' a = HookM () Void Aff a

type HalogenM' res a = H.HalogenM (HookState' res) (HookM' Unit) () Void Aff a

type LogRef = Ref Log

type Log = Array TestEvent

data TestEvent
  = ModifyState
  | RunEffect EffectType
  | RunHooks InterpretHookReason
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
