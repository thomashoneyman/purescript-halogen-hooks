module Test.Setup.Log where

import Prelude

import Data.Array as Array
import Data.Newtype (unwrap)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Aff.Driver.State (DriverState(..))
import Test.Setup.Types (DriverResultState, LogRef, TestEvent, Log)
import Test.Spec.Assertions (shouldEqual)

logShouldBe :: forall r q a. Ref (DriverResultState r q a) -> Log -> Aff Unit
logShouldBe ref x = readLog ref >>= shouldEqual x

unsafeWriteLog :: TestEvent -> LogRef -> Unit
unsafeWriteLog event ref = do
  let _ = unsafePerformEffect $ launchAff_ $ writeLog event ref
  unit

writeLog :: forall m. MonadEffect m => TestEvent -> LogRef -> m Unit
writeLog event ref = liftEffect do
  log <- Ref.read ref
  Ref.write (Array.snoc log event) ref

getLogRef :: forall m r q a. MonadEffect m => Ref (DriverResultState r q a) -> m ( Ref Log )
getLogRef ref = liftEffect do
  DriverState driver <- Ref.read ref
  state <- Ref.read (unwrap driver.state).stateRef
  pure state.input

readLog :: forall m r q a. MonadEffect m => Ref (DriverResultState r q a) -> m Log
readLog ref = liftEffect do
  DriverState driver <- Ref.read ref
  state <- Ref.read (unwrap driver.state).stateRef
  Ref.read state.input

clearLog :: forall m r q a. MonadEffect m => Ref (DriverResultState r q a) -> m Unit
clearLog ref = liftEffect do
  DriverState driver <- Ref.read ref
  state <- Ref.read (unwrap driver.state).stateRef
  Ref.write [] state.input

-- | Useful for logging result state outside of hook evaluation. For example, in
-- | this block we can only access `count` once the actions are completed, but
-- | not in between several actions:
-- |
-- | ```purs`
-- | { count } <- evalM ref do
-- |   { tick } <- initialize
-- |   action tick *> action tick
-- |   finalize
-- | ```
-- |
-- | But with `readResult` we can inspect the value in between actions:
-- |
-- | ```purs
-- | count <- evalM ref do
-- |    { tick } <- initialize
-- |    action tick
-- |    readResult ref >>= logShow
-- |    finalize
-- | ```
readResult :: forall m r q a. MonadEffect m => Ref (DriverResultState r q a) -> m a
readResult ref = liftEffect do
  DriverState driver <- Ref.read ref
  pure $ (unwrap driver.state).result
