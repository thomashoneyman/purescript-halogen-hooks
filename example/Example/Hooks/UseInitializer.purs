module Example.Hooks.UseInitializer
  ( useInitializer
  , UseInitializer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.EvalHookM (EvalHookM)
import Halogen.Hook (Hook, UseEffect)
import Halogen.Hook as Hook

type UseInitializer' hooks = UseEffect hooks

foreign import data UseInitializer :: Type -> Type

useInitializer :: forall ps o m. EvalHookM ps o m Unit -> Hook ps o m UseInitializer Unit
useInitializer eval = Hook.coerce hook
  where
  hook :: Hook ps o m UseInitializer' Unit
  hook = Hook.useLifecycleEffect (eval *> pure Nothing)
