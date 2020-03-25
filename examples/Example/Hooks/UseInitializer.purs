module Example.Hooks.UseInitializer
  ( useInitializer
  , UseInitializer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Halogen.Hooks (HookM, Hook, UseEffect)
import Halogen.Hooks as Hooks

newtype UseInitializer hooks = UseInitializer (UseEffect hooks)

derive instance newtypeUseInitializer :: Newtype (UseInitializer hooks) _

useInitializer
  :: forall slots output m
   . HookM slots output m Unit
  -> Hook slots output m UseInitializer Unit
useInitializer initializer = Hooks.wrap do
  Hooks.useLifecycleEffect (initializer *> pure Nothing)
