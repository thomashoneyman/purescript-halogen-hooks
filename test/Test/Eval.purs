-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Eval where

import Prelude

import Control.Monad.Free (foldFree)
import Data.Array as Array
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (HookM, Hooked(..), UseHookF(..))
import Halogen.Hooks.Component (HookState(..), InterpretHookReason, InternalHookState)
import Halogen.Hooks.Component as Component
import Test.Types (DriverResultState, HookM', HookState', HookType(..), LogRef, TestEvent(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Replacement for `Halogen.Aff.Driver.Eval.evalM`
evalM :: forall r a. Ref (DriverResultState r a) -> H.HalogenM (HookState' a) (HookM' Unit) () Void Aff ~> Aff
evalM initRef = Eval.evalM mempty initRef

interpretUseHookFn
  :: forall hooks q ps o m a
   . InterpretHookReason
  -> (LogRef -> Hooked ps o m Unit hooks a)
  -> H.HalogenM (HookState q LogRef ps o m a) (HookM ps o m Unit) ps o m a
interpretUseHookFn reason hookFn = do
  { input: log } <- Component.getState

  let _ = writeLog RunHooks log
  let Hooked (Indexed hookF) = hookFn log
  a <- foldFree (interpretHook reason hookFn) hookF

  let _ = writeLog Render log
  H.modify_ (over HookState _ { result = a })
  pure a

interpretHook
  :: forall hooks q ps o m a
   . InterpretHookReason
  -> (LogRef -> Hooked ps o m Unit hooks a)
  -> UseHookF ps o m
  ~> H.HalogenM (HookState q LogRef ps o m a) (HookM ps o m Unit) ps o m
interpretHook reason hookFn = case _ of
  c@(UseState initial reply) -> do
    { input: log } <- Component.getState
    let _ = writeLog (EvaluateHook reason UseStateHook) log
    Component.interpretHook reason hookFn c

  c -> do
    { input: log } <- Component.getState
    Component.interpretHook reason hookFn c

-- Create a new DriverState, which can be used to evaluate multiple calls to
-- evaluate test code.
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
      { initialState:
          \_ ->
            HookState
              { result: unit
              , stateRef: unsafePerformEffect $ Ref.new
                  { input: logRef
                  , queryFn: Nothing
                  , stateCells: { queue: [], index: 0 }
                  , effectCells: { queue: [], index: 0 }
                  , memoCells: { queue: [], index: 0 }
                  , refCells: { queue: [], index: 0 }
                  , evalQueue: []
                  }
              }
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

writeLog :: TestEvent -> LogRef -> Unit
writeLog event ref = unsafePerformEffect do
  log <- Ref.read ref
  Ref.write (Array.snoc log event) ref

readLog :: forall r a. Ref (DriverResultState r a) -> Aff (Array TestEvent)
readLog ref = liftEffect do
  DriverState driver <- Ref.read ref

  let
    stateRef :: Ref (InternalHookState _ _ _ _ _ _)
    stateRef = (unwrap driver.state).stateRef

  state <- Ref.read stateRef

  let
    logRef :: Ref (Array TestEvent)
    logRef = state.input

  Ref.read logRef

flushLog :: forall r a. Ref (DriverResultState r a) -> Aff Unit
flushLog ref = liftEffect do
  DriverState driver <- Ref.read ref

  let
    stateRef :: Ref (InternalHookState _ _ _ _ _ _)
    stateRef = (unwrap driver.state).stateRef

  state <- Ref.read stateRef

  let
    logRef :: Ref (Array TestEvent)
    logRef = state.input

  Ref.write [] logRef
