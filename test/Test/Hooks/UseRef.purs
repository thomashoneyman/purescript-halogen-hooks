module Test.Hooks.UseRef where

import Prelude

import Data.Foldable (fold)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Hooks (type (<>), Hook, HookM, UseRef)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, initDriver, mkEval)
import Test.Setup.Log (logShouldBe, readResult)
import Test.Setup.Types (TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Interface = { increment :: HookM Aff Unit, count :: Int }

useRefCount :: Hook Aff (UseRef Int <> Hooks.Pure) Interface
useRefCount = Hooks.do
  count /\ countRef <- Hooks.useRef 0
  Hooks.pure { count, increment: liftEffect $ Ref.modify_ (_ + 1) countRef }

refHook :: Spec Unit
refHook = before initDriver $ describe "useRef" do
  let eval = mkEval (const useRefCount)

  it "initializes to the proper initial value" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize
      readResult ref

    count `shouldEqual` 0

  it "updates state in response to actions" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize

      { increment } <- readResult ref
      eval (H.Action increment)
      eval (H.Action increment)
      eval (H.Action increment)

      eval H.Finalize
      readResult ref

    count `shouldEqual` 3

  it "does not cause re-evaluation when value updates" \ref -> do
    { count } <- evalM ref do
      eval H.Initialize

      { increment } <- readResult ref
      eval (H.Action increment)
      eval (H.Action increment)
      eval (H.Action increment)
      eval (H.Action increment)

      eval H.Finalize
      readResult ref

    count `shouldEqual` 4

    -- despite multiple increments there should be no hook evaluation outside
    -- of the initializer and finalizer
    logShouldBe ref $ fold [ initializeSteps, finalizeSteps ]

  where
  initializeSteps =
    [ RunHooks Initialize, Render ]

  finalizeSteps =
    [ RunHooks Finalize, Render ]
