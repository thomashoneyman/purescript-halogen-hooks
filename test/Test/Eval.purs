module Test.Eval where

import Prelude

import Control.Monad.Free (foldFree)
import Control.Monad.Writer (WriterT, tell)
import Data.Array as Array
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Indexed (Indexed(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen (gets)
import Halogen as H
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (Hook, HookF, HookM(..), Hooked(..), StateToken(..), UseHookF(..))
import Halogen.Hooks as HookM
import Halogen.Hooks.Component (HookState(..), InternalHookState, InterpretHookReason(..), initialState, unsafeGetCell, unsafeSetCell)
import Partial.Unsafe (unsafeCrashWith)
import Test.TestM (TestF(..), TestM(..))
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

data TestEvent
  = GetState
  | ModifyState

derive instance eqTestEvent :: Eq TestEvent
derive instance ordTestEvent :: Ord TestEvent
derive instance genericTestEvent :: Generic TestEvent _

instance showTestEvent :: Show TestEvent where
  show = genericShow

type DriverState' r = DriverState HH.HTML r HookState' (Const Void) Unit () Unit Void
type HookState' = HookState (Const Void) Unit () Void Aff

evalTestM
  :: forall r
   . Ref (DriverState' r)
  -> TestM HookState' Aff
  ~> Aff
evalTestM initRef (TestM testM) = foldFree (go initRef) testM
  where
  go
    :: Ref (DriverState' r)
    -> TestF HookState' Aff
    ~> Aff
  go ref = case _ of
    State f -> do
      DriverState (st@{ state }) <- liftEffect (Ref.read ref)
      case f state of
        Tuple a state'
          | unsafeRefEq state state' -> pure a
          | otherwise -> do
              liftEffect $ Ref.write (DriverState (st { state = state' })) ref
              -- handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
              pure a

    Lift aff -> aff

evalTestHookM
  :: forall q i ps o m
   . HookM ps o m
  ~> WriterT (Array TestEvent) (TestM (HookState q i ps o m) m)
evalTestHookM (HookM hm) = foldFree go hm
  where
  go :: HookF ps o m ~> WriterT (Array TestEvent) (TestM (HookState q i ps o m) m)
  go = case _ of
    HookM.Modify (StateToken token) f reply -> do
      state <- getState
      let
        v = f (unsafeGetCell token state.stateCells.queue)
        newState =
          state { stateCells { queue = unsafeSetCell token v state.stateCells.queue } }
      modifyState_ (const newState)
      tell [ ModifyState ]
      pure (reply v)

    _ ->
      unsafeCrashWith "not implemented"

evalTestHook
  :: forall q i ps o m h
   . InterpretHookReason
  -> Hook ps o m h
  ~> WriterT (Array TestEvent) (TestM (HookState q i ps o m) m)
evalTestHook reason (Hooked (Indexed hookF)) = foldFree (go reason) hookF
  where
  go
    :: InterpretHookReason
    -> UseHookF ps o m
    ~> WriterT (Array TestEvent) (TestM (HookState q i ps o m) m)
  go _ = case _ of
    UseState initial reply ->
      case reason of
        Initialize -> do
          { stateCells: { queue } } <- getState

          let
            newQueue = Array.snoc queue initial
            token = StateToken (Array.length newQueue - 1)

          modifyState_ _ { stateCells { queue = newQueue } }
          pure $ reply $ Tuple initial token

        _ -> do
          { stateCells: { index, queue } } <- getState

          let
            value = unsafeGetCell index queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            token = StateToken index

          modifyState_ _ { stateCells { index = nextIndex } }
          pure $ reply $ Tuple value token

    _ ->
      unsafeCrashWith "not implemented"

-- Create a new DriverState, which can be used to evaluate multiple calls to
-- evaluate test code.
initDriver
  :: forall r s q act ps i o
   . Aff (Ref (DriverState HH.HTML r s q act ps i o))
initDriver = do
  lifecycleRef <- liftEffect $ Ref.new mempty
  initRef <-
    liftEffect $ initDriverState
      { initialState
      , render: \(HookState { html }) -> html
      , eval: H.mkEval H.defaultEval
      }
      unit
      (const (pure unit))
      lifecycleRef

  pure (unDriverStateXRef initRef)

  where
  unDriverStateXRef
    :: forall r' s' f' act' ps' i' o'
    . Ref (DriverStateX HH.HTML r' f' o')
    -> Ref (DriverState HH.HTML r' s' f' act' ps' i' o')
  unDriverStateXRef = unsafeCoerce

getState
  :: forall q i ps o m
   . WriterT (Array TestEvent) (TestM (HookState q i ps o m) m) (InternalHookState q i ps o m)
getState = do
  { stateRef } <- gets unwrap
  let state = unsafePerformEffect $ liftEffect $ Ref.read stateRef
  pure state

modifyState_
  :: forall q i ps o m
   . (InternalHookState q i ps o m -> InternalHookState q i ps o m)
  -> WriterT (Array TestEvent) (TestM (HookState q i ps o m) m) Unit
modifyState_ fn = do
  { stateRef } <- gets unwrap
  let state = unsafePerformEffect $ liftEffect $ Ref.modify_ fn stateRef
  pure state
