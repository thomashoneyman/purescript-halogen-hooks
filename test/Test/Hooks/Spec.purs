module Test.Hooks.Spec where

import Prelude

import Test.Hooks.UseLifecycleEffect (lifecycleEffectHook)
import Test.Hooks.UseMemo (memoHook)
import Test.Hooks.UseRef (refHook)
import Test.Hooks.UseState (stateHook)
import Test.Hooks.UseTickEffect (tickEffectHook)
import Test.Spec (Spec)

spec :: Spec Unit
spec = do
  stateHook
  tickEffectHook
  lifecycleEffectHook
  memoHook
  refHook
