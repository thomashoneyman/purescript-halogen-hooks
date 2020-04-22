module Test.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Halogen.Hooks (Hook, HookF, HookM, UseHookF)
import Halogen.Hooks.Component (InterpretHookReason)

data TestEvent
  = ModifyState
  | RunHooks InterpretHookReason
  | Render

derive instance eqTestEvent :: Eq TestEvent
derive instance genericTestEvent :: Generic TestEvent _

instance showTestEvent :: Show TestEvent where
  show = genericShow

type UseHookF' = UseHookF () Void Aff

type HookF' = HookF () Void Aff

type HookM' = HookM () Void Aff

type Hook' hookType a = Hook () Void Aff hookType a
