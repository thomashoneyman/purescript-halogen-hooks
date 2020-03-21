module Example.Hooks.UseInitializer
  ( useInitializer
  , UseInitializer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Hooks (HalogenHookM, Hook, UseEffect)
import Halogen.Hooks as Hooks

type UseInitializer' hooks = UseEffect hooks

foreign import data UseInitializer :: Type -> Type

useInitializer :: forall ps o m. HalogenHookM ps o m Unit -> Hook ps o m UseInitializer Unit
useInitializer eval = Hooks.coerce hook
  where
  hook :: Hook ps o m UseInitializer' Unit
  hook = Hooks.useLifecycleEffect (eval *> pure Nothing)
