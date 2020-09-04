module Test.Integration.Spec where

import Prelude

import Test.Integration.Issue5 (rerunTickAfterInitialEffectsHook)
import Test.Spec (Spec)

spec :: Spec Unit
spec = do
  rerunTickAfterInitialEffectsHook
