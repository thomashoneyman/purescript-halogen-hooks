module Test.Hooks.UseMemo where

import Prelude

import Data.Foldable (fold)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Hooks (HookM, UseMemo, UseState, Hook)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Internal.Eval.Types (InterpretHookReason(..))
import Test.Setup.Eval (evalM, mkEval, initDriver)
import Test.Setup.Log (logShouldBe, readResult, unsafeWriteLog)
import Test.Setup.Types (LogRef, MemoType(..), TestEvent(..))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype MemoHook h =
  MemoHook (UseMemo Int (UseMemo Int (UseMemo Int (UseState Int (UseState Int (UseState Int h))))))

derive instance newtypeMemoHook :: Newtype (MemoHook h) _

type MemoCount =
  { incrementA :: HookM Aff Unit
  , incrementB :: HookM Aff Unit
  , incrementC :: HookM Aff Unit
  , expensive1 :: Int
  , expensive2 :: Int
  , expensive3 :: Int
  }

useMemoCount :: LogRef -> Hook Aff MemoHook MemoCount
useMemoCount log = Hooks.wrap Hooks.do
  state1 /\ modifyState1 <- Hooks.useState 0
  state2 /\ modifyState2 <- Hooks.useState 0
  state3 /\ modifyState3 <- Hooks.useState 0

  expensive1 <- memoize1 { state1 }
  expensive2 <- memoize2 { state2 }
  expensive3 <- memoize3 { state1, state2 }

  Hooks.pure
    { incrementA: modifyState1 (_ + 1) -- recomputes 1 and 3
    , incrementB: modifyState2 (_ + 1) -- recomputes 2 and 3
    , incrementC: modifyState3 (_ + 1) -- recomputes nothing
    , expensive1
    , expensive2
    , expensive3
    }
  where
  memoize1 deps@{ state1 } = Hooks.captures deps $ flip Hooks.useMemo \_ -> do
    let _ = unsafeWriteLog (RunMemo (CalculateMemo 1)) log
    state1 + 5

  memoize2 deps@{ state2 } = Hooks.captures deps $ flip Hooks.useMemo \_ -> do
    let _ = unsafeWriteLog (RunMemo (CalculateMemo 2)) log
    state2 + 5

  memoize3 deps@{ state1, state2 } = Hooks.captures deps $ flip Hooks.useMemo \_ -> do
    let _ = unsafeWriteLog (RunMemo (CalculateMemo 3)) log
    state1 + state2 + 5

memoHook :: Spec Unit
memoHook = before initDriver $ describe "useMemo" do
  let eval = mkEval useMemoCount

  it "initializes to the proper initial values" \ref -> do
    { expensive1, expensive2, expensive3 } <- evalM ref do
      eval H.Initialize
      readResult ref

    expensive1 `shouldEqual` 5
    expensive2 `shouldEqual` 5
    expensive3 `shouldEqual` 5

  it "recalculates memoized values in response to actions" \ref -> do
    { expensive1, expensive2, expensive3 } <- evalM ref do
      eval H.Initialize

      { incrementA, incrementB } <- readResult ref
      eval (H.Action incrementA) *> eval (H.Action incrementB)

      eval H.Finalize
      readResult ref

    expensive1 `shouldEqual` 6
    expensive2 `shouldEqual` 6
    expensive3 `shouldEqual` 7

    logShouldBe ref $ fold
      [ initializeSteps
        -- incrementA should recompute memos 1 and 3
      , [ ModifyState, RunHooks Step, RunMemo (CalculateMemo 1), RunMemo (CalculateMemo 3), Render ]
        -- incrementB should recompute memos 2 and 3
      , [ ModifyState, RunHooks Step, RunMemo (CalculateMemo 2), RunMemo (CalculateMemo 3), Render ]

      , finalizeSteps
      ]

  it "does not recalculate memoized values when memos are unchanged" \ref -> do
    { expensive1, expensive2, expensive3 } <- evalM ref do
      eval H.Initialize

      { incrementC } <- readResult ref
      eval (H.Action incrementC)

      eval H.Finalize
      readResult ref

    expensive1 `shouldEqual` 5
    expensive2 `shouldEqual` 5
    expensive3 `shouldEqual` 5

    logShouldBe ref $ fold
      [ initializeSteps
      , [ ModifyState, RunHooks Step, Render ] -- incrementC shouldn't affect memoized values
      , finalizeSteps
      ]

  where
  initializeSteps =
    [ RunHooks Initialize, RunMemo (CalculateMemo 1), RunMemo (CalculateMemo 2), RunMemo (CalculateMemo 3), Render ]

  finalizeSteps =
    [ RunHooks Finalize, Render ]
