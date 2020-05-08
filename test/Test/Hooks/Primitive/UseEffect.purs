module Test.Hooks.Primitive.UseEffect where

import Prelude

import Test.Hooks.Primitive.UseEffect.UseLifecycleEffect (lifecycleEffectHook)
import Test.Hooks.Primitive.UseEffect.UseTickEffect (tickEffectHook)
import Test.Spec (Spec, describe)

effectHook :: Spec Unit
effectHook = describe "useEffect" do
  lifecycleEffectHook
  tickEffectHook
