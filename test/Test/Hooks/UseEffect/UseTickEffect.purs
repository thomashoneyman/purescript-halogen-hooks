module Test.Hooks.UseEffect.UseTickEffect where

import Prelude

import Test.Spec (Spec, describe, pending)

tickEffectHook :: Spec Unit
tickEffectHook = describe "useTickEffect" do
  pending "effect body runs on state change"
  pending "effect cleanup runs on state change"
  pending "effect is run when memos change"
  pending "effect is skipped when memos are unchanged"
