module Test.Hooks.UseRef where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Ref as Ref
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Hooks (UseRef)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, mkEval)
import Test.Setup.Log (initDriver, logShouldBe, readResult)
import Test.Setup.Types (Hook', HookM', LogRef, TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

useRefCount :: LogRef -> Hook' (UseRef Int) { increment :: HookM' Unit, count :: Int }
useRefCount ref = Hooks.do
  count /\ countRef <- Hooks.useRef 0
  Hooks.pure { count, increment: liftEffect $ Ref.modify_ (_ + 1) countRef }

refHook :: Spec Unit
refHook = before initDriver $ describe "useRef" do
  let eval = mkEval useRefCount

  it "initializes to the proper initial value" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      liftAff $ readResult ref

    count `shouldEqual` 0

  it "updates state in response to actions" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      { increment } <- liftAff $ readResult ref

      eval (H.tell $ H.Action increment)
        *> eval (H.tell $ H.Action increment)
        *> eval (H.tell $ H.Action increment)

      eval $ H.tell $ H.Finalize
      liftAff $ readResult ref

    count `shouldEqual` 3

  it "does not cause re-evaluation when value updates" \ref -> do
    { count } <- evalM ref do
      eval $ H.tell H.Initialize
      { increment } <- liftAff $ readResult ref

      eval (H.tell $ H.Action increment)
        *> eval (H.tell $ H.Action increment)
        *> eval (H.tell $ H.Action increment)
        *> eval (H.tell $ H.Action increment)

      eval $ H.tell $ H.Finalize
      liftAff $ readResult ref

    count `shouldEqual` 4
    logShouldBe ref do
      [ RunHooks Initialize
      , Render

      -- despite multiple increments there should be no further hook evaluation

      , RunHooks Finalize
      , Render
      ]
