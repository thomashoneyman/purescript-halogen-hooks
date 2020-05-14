module Example.Hooks.UseInitializer
  ( useInitializer
  , UseInitializer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Hooks (class HookNewtype, Hook, HookM, UseEffect)
import Halogen.Hooks as Hooks

foreign import data UseInitializer :: Hooks.HookType

instance hookUseInitializer :: HookNewtype UseInitializer UseEffect

useInitializer :: forall m. HookM m Unit -> Hook m UseInitializer Unit
useInitializer initializer = Hooks.wrap hook
  where
  hook :: Hook m UseEffect Unit
  hook = Hooks.useLifecycleEffect (initializer *> pure Nothing)
