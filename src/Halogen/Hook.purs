module Halogen.Hook
  ( useState
  , UseState
  , useEval
  , UseEval
  , Hook
  , Hooked
  , Action
  , component
  , defaultEval
  , bind
  , discard
  , pure
  )
where

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Indexed (class IxMonad)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM (imapState, mapAction)
import Partial.Unsafe (unsafePartial)
import Prelude (class Functor, type (~>), Unit, const, map, mempty, unit, ($), (+), (<), (<<<))
import Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | The Hook API: a set of primitive building blocks that can be used on their
-- | own to share stateful logic or used to create new hooks.
data HookF out m a
  = UseState StateValue (StateInterface out m -> a)
  | UseEval StateValue (ConcreteEvalSpec out m) (EvalInterface out m -> a)

derive instance functorHookF :: Functor (HookF out m)

newtype Hooked out m i o a = Hooked (Indexed (Free (HookF out m)) i o a)

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked out m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked out m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked out m)
derive newtype instance ixBindIndexed :: IxBind (Hooked out m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked out m)

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

type Hook out m (newHook :: Type -> Type) a
  = forall hooks. Hooked out m hooks (newHook hooks) a

--  State

-- Must structurally match state
foreign import data UseState :: Type -> Type -> Type

foreign import data StateValue :: Type

toStateValue :: forall state. state -> StateValue
toStateValue = unsafeCoerce

fromStateValue :: forall state. StateValue -> state
fromStateValue = unsafeCoerce

type StateInterface out m =
  { setState :: StateValue -> Action out m
  , getState :: StateValue
  }

useState
  :: forall state out m
   . state
  -> Hook out m (UseState state) (Tuple state (state -> Action out m))
useState initialState = Hooked $ Indexed $ liftF $ UseState initialStateValue hookInterface
  where
  initialStateValue :: StateValue
  initialStateValue = toStateValue initialState

  hookInterface :: StateInterface out m -> Tuple state (state -> Action out m)
  hookInterface { getState, setState } = Tuple (fromStateValue getState) (setState <<< toStateValue)

-- Action

-- Must match state, action, slots, out, and monad
foreign import data UseEval :: Type -> Type -> # Type -> Type -> (Type -> Type) -> Type ->  Type

foreign import data ActionValue :: Type

toActionValue :: forall action. action -> ActionValue
toActionValue = unsafeCoerce

fromActionValue :: forall action. ActionValue -> action
fromActionValue = unsafeCoerce

foreign import data SlotValue :: # Type

hideSlotsHalogenM
  :: forall state action slots out m
   . H.HalogenM state action slots out m
  ~> H.HalogenM state action SlotValue out m
hideSlotsHalogenM m = unsafeCoerce m

hideSlotsComponentHTML
  :: forall action slots m
   . H.ComponentHTML action slots m
  -> H.ComponentHTML action SlotValue m
hideSlotsComponentHTML m = unsafeCoerce m

-- | Like a Halogen EvalSpec type, but without a receiver (input is handled
-- | differently in this Hook implementation)
type EvalSpec state action slots out m =
  { initialize :: Maybe action
  , handleAction :: action -> H.HalogenM state action slots out m Unit
  , finalize :: Maybe action
  }

defaultEval :: forall state action slots out m. EvalSpec state action slots out m
defaultEval =
  { initialize: Nothing
  , handleAction: mempty
  , finalize: Nothing
  }

type ConcreteEvalSpec out m =
  { handleAction :: ActionValue -> H.HalogenM StateValue ActionValue SlotValue out m Unit
  , initialize :: Maybe ActionValue
  , finalize :: Maybe ActionValue
  }

type EvalInterface out m =
  { getState :: StateValue
  , runAction :: ActionValue -> Action out m
  }

useEval
  :: forall state action slots out m
   . Functor m
  => state
  -> EvalSpec state action slots out m
  -> Hook out m (UseEval state action slots out m) (Tuple state (action -> Action out m))
useEval initialState spec =
  Hooked $ Indexed $ liftF $ UseEval initialStateValue concreteSpec hookInterface
  where
  initialStateValue :: StateValue
  initialStateValue = toStateValue initialState

  concreteSpec :: ConcreteEvalSpec out m
  concreteSpec =
    { initialize: map toActionValue spec.initialize
    , handleAction: convertHalogenM <<< spec.handleAction <<< fromActionValue
    , finalize: map toActionValue spec.finalize
    }

  convertHalogenM :: H.HalogenM state action slots out m ~> H.HalogenM StateValue ActionValue SlotValue out m
  convertHalogenM = hideSlotsHalogenM <<< mapAction toActionValue <<< imapState toStateValue fromStateValue

  hookInterface :: EvalInterface out m -> Tuple state (action -> Action out m)
  hookInterface { getState, runAction } =
    Tuple (fromStateValue getState) (runAction <<< toActionValue)

-- Underlying component

type HookState input out m =
  { state :: QueueState
  , html :: H.ComponentHTML (Action out m) SlotValue m
  , input :: input
  }

type QueueState =
  { queue :: Array StateValue
  , total :: Int
  , index :: Int
  }

-- An identifier for to a piece of state. A user should not be able to
-- construct this and it is not exported.
newtype StateId = StateId Int

-- Effects that the user ought to be able to run in this component are
-- represented as actions with accompanying handlers. These actions need
-- a reference to the piece of state they are acting on, but that reference
-- shouldn't be constructable by the end user.
data Action out m
  = ModifyState StateId StateValue
  | RunAction StateId (ActionValue -> H.HalogenM StateValue ActionValue SlotValue out m Unit) ActionValue

handleAction
  :: forall input out hooks m
   . Functor m
  => (HookF out m ~> H.HalogenM (HookState input out m) (Action out m) SlotValue out m)
  -> (input -> Hooked out m Unit hooks (H.ComponentHTML (Action out m) SlotValue m))
  -> Action out m
  -> H.HalogenM (HookState input out m) (Action out m) SlotValue out m Unit
handleAction interpreter hookFn = case _ of
  ModifyState stateId newState -> Prelude.do
    { input, state} <- H.get
    H.modify_ _ { state { queue = unsafeSetState stateId newState state.queue } }

    let Hooked (Indexed hookF) = hookFn input
    html <- foldFree interpreter hookF
    H.modify_ _ { html = html }

  RunAction stateId handler action -> Prelude.do
    current <- H.get

    let
      toState :: StateValue -> HookState input out m
      toState stateValue =
        current { state { queue = unsafeSetState stateId stateValue current.state.queue } }

      fromState :: HookState input out m -> StateValue
      fromState = \state -> unsafeGetState stateId state.state.queue

    -- Run the action
    (mapAction (RunAction stateId handler)) (imapState toState fromState (handler action))

    let Hooked (Indexed hookF) = hookFn current.input
    html <- foldFree interpreter hookF
    H.modify_ _ { html = html }

data InterpretHookReason
  = Initialize
  | Step
  | Finalize

component
  :: forall hooks slots q i o m
   . Functor m
  => (i -> Hooked o m Unit hooks (H.ComponentHTML (Action o m) slots m))
  -> H.Component HH.HTML q i o m
component hookFn' = do
  let
    -- Hide the slot value internally.
    hookFn :: i -> Hooked o m Unit hooks (H.ComponentHTML (Action o m) SlotValue m)
    hookFn i = do
      let (Hooked (Indexed x)) = hookFn' i
      Hooked $ Indexed $ map hideSlotsComponentHTML x

  H.mkComponent
    { initialState
    , render: _.html
    , eval: case _ of
        H.Initialize a -> Prelude.do
          runInterpreter Initialize hookFn
          Prelude.pure a

        H.Query query reply -> Prelude.do
          -- runInterpreter (Query query reply) hookFn
          Prelude.pure (reply unit)

        H.Action act a -> Prelude.do
          handleAction (interpretHook Step hookFn) hookFn act
          Prelude.pure a

        H.Receive input a -> Prelude.do
          H.modify_ _ { input = input }
          runInterpreter Step hookFn
          Prelude.pure a

        H.Finalize a -> Prelude.do
          runInterpreter Finalize hookFn
          Prelude.pure a
    }
  where
  initialState :: i -> HookState i o m
  initialState input =
    { html: HH.text ""
    , state: { queue: [], total: 0, index: 0 }
    , input
    }

runInterpreter
  :: forall i o m hooks
   . Functor m
  => InterpretHookReason
  -> (i -> Hooked o m Unit hooks (H.ComponentHTML (Action o m) SlotValue m))
  -> H.HalogenM (HookState i o m) (Action o m) SlotValue o m Unit
runInterpreter reason hookFn = Prelude.do
  { input } <- H.get

  let
    Hooked (Indexed hookF) = hookFn input
    interpret = interpretHook reason hookFn

  html <- foldFree interpret hookF
  H.modify_ _ { html = html }
  Prelude.pure unit

interpretHook
  :: forall i o m hooks
   . Functor m
  => InterpretHookReason
  -> (i -> Hooked o m Unit hooks (H.ComponentHTML (Action o m) SlotValue m))
  -> HookF o m
  ~> H.HalogenM (HookState i o m) (Action o m) SlotValue o m
interpretHook reason hookFn = case _ of
  UseState initial reply ->
    case reason of
      Initialize -> Prelude.do
        { id } <- initializeHook initial
        Prelude.pure $ reply { getState: initial, setState: ModifyState id }

      _ -> Prelude.do
        { state, id } <- stepHook
        Prelude.pure $ reply { getState: state, setState: ModifyState id }

  UseEval initial spec reply ->
    case reason of
      Initialize -> Prelude.do
        { id } <- initializeHook initial
        for_ spec.initialize \act ->
          handleAction (interpretHook Step hookFn) hookFn (RunAction id spec.handleAction act)
        Prelude.pure $ reply { getState: initial, runAction: RunAction id spec.handleAction }

      Step -> Prelude.do
        { state, id } <- stepHook
        Prelude.pure $ reply { getState: state, runAction: RunAction id spec.handleAction }

      Finalize -> Prelude.do
        { state, id } <- stepHook
        for_ spec.finalize \act ->
          handleAction (interpretHook Step hookFn) hookFn (RunAction id spec.handleAction act)
        Prelude.pure $ reply { getState: state, runAction: RunAction id spec.handleAction }

initializeHook :: forall i a s o m. StateValue -> H.HalogenM (HookState i o m) a s o m { id :: StateId }
initializeHook initialState = Prelude.do
  { state } <- H.get

  let
    queue =
      { queue: Array.snoc state.queue initialState
      , index: 0
      , total: state.total + 1
      }

  H.modify_ _ { state = queue }
  Prelude.pure { id: StateId state.total }

stepHook :: forall i a s o m. H.HalogenM (HookState i o m) a s o m { state :: StateValue, id :: StateId }
stepHook = Prelude.do
  { state } <- H.get

  let
    stateValue = unsafeGetState (StateId state.index) state.queue
    nextIndex =
      if state.index + 1 < state.total
        then state.index + 1
        else 0

    queue =
      { queue: state.queue
      , index: nextIndex
      , total: state.total
      }

  H.modify_ _ { state = queue }
  Prelude.pure { state: stateValue, id: StateId state.index }

-- Utilities for updating state

unsafeGetState :: StateId -> Array StateValue -> StateValue
unsafeGetState (StateId index) array = unsafePartial (Array.unsafeIndex array index)

unsafeSetState :: StateId -> StateValue -> Array StateValue -> Array StateValue
unsafeSetState (StateId index) a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))
