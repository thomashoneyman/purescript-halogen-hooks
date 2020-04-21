module Test.Hooks.UseEffect where

import Prelude

import Test.Hooks.UseEffect.UseLifecycleEffect (lifecycleEffectHook)
import Test.Hooks.UseEffect.UseTickEffect (tickEffectHook)
import Test.Spec (Spec, describe)

effectHook :: Spec Unit
effectHook = describe "useEffect" do
  lifecycleEffectHook
  tickEffectHook
