module Example.Hooks.UseInitializer
  ( useInitializer
  , UseInitializer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Hooks (HookM, Hook, UseEffect)
import Halogen.Hooks as Hooks

type UseInitializer' hooks = UseEffect hooks

foreign import data UseInitializer :: Type -> Type

useInitializer
  :: forall slots output m
   . HookM slots output m Unit
  -> Hook slots output m UseInitializer Unit
useInitializer initializer = Hooks.publish hook
  where
  hook :: Hook slots output m UseInitializer' Unit
  hook = Hooks.useLifecycleEffect (initializer *> pure Nothing)
