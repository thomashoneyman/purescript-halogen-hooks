module Test.Types where

import Prelude

import Control.Monad.Writer (WriterT)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Halogen.Aff.Driver.State (DriverState)
import Halogen.HTML as HH
import Halogen.Hooks (Hook, HookF, HookM, UseHookF)
import Halogen.Hooks.Component (HookState, InternalHookState)
import Test.TestM (TestF, TestM)

type TestWriterM = WriterT (Array TestEvent) TestM'

data TestEvent
  = GetState
  | ModifyState

derive instance eqTestEvent :: Eq TestEvent
derive instance ordTestEvent :: Ord TestEvent
derive instance genericTestEvent :: Generic TestEvent _

instance showTestEvent :: Show TestEvent where
  show = genericShow

type InternalHookState' = InternalHookState (Const Void) Unit () Void Aff

type HookState' = HookState (Const Void) Unit () Void Aff

type DriverState' r = DriverState HH.HTML r HookState' (Const Void) Unit () Unit Void

type UseHookF' = UseHookF () Void Aff

type HookF' = HookF () Void Aff

type HookM' = HookM () Void Aff

type Hook' hookType a = Hook () Void Aff hookType a

type TestF' = TestF HookState' Aff

type TestM' = TestM HookState' Aff
