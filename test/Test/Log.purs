module Test.Log where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks.Component (HookState(..))
import Test.Spec.Assertions (shouldEqual)
import Test.Types (DriverResultState, LogRef, TestEvent, Log)
import Unsafe.Coerce (unsafeCoerce)

logShouldBe :: forall r a. Ref (DriverResultState r a) -> Log -> Aff Unit
logShouldBe ref x = readLog ref >>= shouldEqual x

writeLog :: TestEvent -> LogRef -> Aff Unit
writeLog event ref = liftEffect do
  log <- Ref.read ref
  Ref.write (Array.snoc log event) ref

readLog :: forall r a. Ref (DriverResultState r a) -> Aff Log
readLog ref = liftEffect do
  DriverState driver <- Ref.read ref
  state <- Ref.read (unwrap driver.state).stateRef
  Ref.read state.input

clearLog :: forall r a. Ref (DriverResultState r a) -> Aff Unit
clearLog ref = liftEffect do
  DriverState driver <- Ref.read ref
  state <- Ref.read (unwrap driver.state).stateRef
  Ref.write [] state.input

-- | Create a new DriverState, which can be used to evaluate multiple calls to
-- | evaluate test code, and which contains the LogRef.
initDriver :: forall r a. Aff (Ref (DriverResultState r a))
initDriver = liftEffect do
  logRef <- Ref.new []

  stateRef <- Ref.new
    { input: logRef
    , queryFn: Nothing
    , stateCells: { queue: [], index: 0 }
    , effectCells: { queue: [], index: 0 }
    , memoCells: { queue: [], index: 0 }
    , refCells: { queue: [], index: 0 }
    , evalQueue: []
    }

  lifecycleHandlers <- Ref.new mempty

  map unDriverStateXRef do
    initDriverState
      { initialState: \_ -> HookState { result: unit, stateRef }
      , render: \_ -> HH.text ""
      , eval: H.mkEval H.defaultEval
      }
      unit
      mempty
      lifecycleHandlers
  where
  unDriverStateXRef
    :: forall r' s' f' act' ps' i' o'
     . Ref (DriverStateX HH.HTML r' f' o')
    -> Ref (DriverState HH.HTML r' s' f' act' ps' i' o')
  unDriverStateXRef = unsafeCoerce
