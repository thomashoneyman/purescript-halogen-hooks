module Halogen.Hook 
  ( useState 
  , UseState
  , useAction
  , UseAction
  , Hook
  , Hooked
  , Action
  , component
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
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM (imapState, mapAction)
import Partial.Unsafe (unsafePartial)
import Prelude (Void, Unit, class Functor, type (~>), (<<<), ($), unit, const, map, (*>), (+), (<))
import Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | The Hook API: a set of primitive building blocks that can be used on their
-- | own to share stateful logic or used to create new hooks.
data HookF a
  = UseState (Unit -> StateValue) (StateInterface -> a)
  | UseAction ActionHandler (Unit -> StateValue) (Tuple StateValue (ActionValue -> Action) -> a)

derive instance functorHookF :: Functor HookF

newtype Hooked i o a = Hooked (Indexed (Free HookF) i o a)

derive newtype instance ixFunctorIndexed :: IxFunctor Hooked
derive newtype instance ixApplyIndexed :: IxApply Hooked
derive newtype instance ixApplicativeIndexed :: IxApplicative Hooked
derive newtype instance ixBindIndexed :: IxBind Hooked
derive newtype instance ixMonadIndexed :: IxMonad Hooked

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

type Hook (newHook :: Type -> Type) a
  = forall hooks. Hooked hooks (newHook hooks) a

--  State

foreign import data UseState :: Type -> Type -> Type

foreign import data StateValue :: Type

toStateValue :: forall state. state -> StateValue
toStateValue = unsafeCoerce

fromStateValue :: forall state. StateValue -> state
fromStateValue = unsafeCoerce

type StateInterface =
  { set :: StateValue -> Action
  , get :: StateValue
  }

type State state = Tuple state (state -> Action)

useState :: forall state. (Unit -> state) -> Hook (UseState state) (State state)
useState initialState = Hooked $ Indexed $ liftF $ UseState initialStateValue hookInterface
  where
  initialStateValue :: Unit -> StateValue
  initialStateValue = map toStateValue initialState

  hookInterface :: StateInterface -> State state
  hookInterface { get, set } = Tuple (fromStateValue get) (set <<< toStateValue) 

-- Action

foreign import data UseAction :: Type -> Type -> Type -> Type

foreign import data ActionValue :: Type

toActionValue :: forall action. action -> ActionValue
toActionValue = unsafeCoerce

fromActionValue :: forall action. ActionValue -> action
fromActionValue = unsafeCoerce

type ActionHandler = ActionValue -> H.HalogenM StateValue ActionValue () Void Aff Unit

useAction 
  :: forall state action
   . (action -> H.HalogenM state action () Void Aff Unit)
  -> (Unit -> state) 
  -> Hook (UseAction state action) (Tuple state (action -> Action))
useAction handler initialState = Hooked $ Indexed $ liftF $ UseAction hookHandler initialStateValue hookInterface
  where
  initialStateValue :: Unit -> StateValue
  initialStateValue = Prelude.map toStateValue initialState

  hookHandler :: ActionValue -> H.HalogenM StateValue ActionValue () Void Aff Unit
  hookHandler actionValue = do
    let 
      handler' :: H.HalogenM StateValue action () Void Aff Unit 
      handler' = imapState toStateValue fromStateValue (handler (fromActionValue actionValue))

    mapAction toActionValue handler'

  hookInterface :: Tuple StateValue (ActionValue -> Action) -> Tuple state (action -> Action)
  hookInterface (Tuple st fn) = Tuple (fromStateValue st) (fn <<< toActionValue)

-- Underlying component

type HookState = 
  { state :: QueueState
  , html :: H.ComponentHTML Action () Aff
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
data Action
  = ModifyState StateId StateValue
  | RunAction StateId ActionHandler ActionValue

handleAction
  :: forall hooks
   . Hooked Unit hooks (H.ComponentHTML Action () Aff)
  -> (HookF ~> H.HalogenM HookState Action () Void Aff)
  -> Action 
  -> H.HalogenM HookState Action () Void Aff Unit
handleAction (Hooked (Indexed hookF)) interpretHook = case _ of
  ModifyState stateId newState -> Prelude.do
    { state} <- H.get
    H.modify_ _ { state { queue = unsafeSetState stateId newState state.queue } }

    html <- foldFree interpretHook hookF
    H.modify_ _ { html = html }

  RunAction stateId handler action -> Prelude.do
    current <- H.get

    let 
      toState :: StateValue -> HookState
      toState stateValue =
        current { state { queue = unsafeSetState stateId stateValue current.state.queue } }

      fromState :: HookState -> StateValue
      fromState = \state -> unsafeGetState stateId state.state.queue

    -- Run the action
    (mapAction (RunAction stateId handler)) (imapState toState fromState (handler action))

    html <- foldFree interpretHook hookF
    H.modify_ _ { html = html }

component :: forall hooks q i. Hooked Unit hooks (H.ComponentHTML Action () Aff) -> H.Component HH.HTML q i Void Aff
component hook@(Hooked (Indexed hookF)) =
  H.mkComponent
    { initialState: \_ -> { html: HH.text "", state: { queue: [], total: 0, index: 0 } }
    , render: _.html
    , eval: case _ of
        Initialize a -> Prelude.do
          html <- foldFree (interpretHook true) hookF
          H.modify_ _ { html = html }
          Prelude.pure a

        Query _ reply -> 
          Prelude.pure (reply unit)

        Action act a -> 
          handleAction hook (interpretHook false) act *> Prelude.pure a

        Receive _ a -> 
          Prelude.pure a

        Finalize a -> 
          Prelude.pure a
    }
  where
  interpretHook :: Boolean -> HookF ~> H.HalogenM HookState Action () Void Aff
  interpretHook isInitial = case _ of
    UseState initialState reply ->
      if isInitial then Prelude.do 
        { state, id } <- initializeHook initialState
        Prelude.pure $ reply { get: state, set: ModifyState id }
      
      else Prelude.do
        { state, id } <- stepHook
        Prelude.pure $ reply { get: state, set: ModifyState id }

    UseAction handler initialState reply -> Prelude.do
      if isInitial then Prelude.do
        { state, id } <- initializeHook initialState
        Prelude.pure $ reply $ Tuple state (RunAction id handler)

      else Prelude.do
        { state, id } <- stepHook
        Prelude.pure $ reply $ Tuple state (RunAction id handler)

initializeHook 
  :: forall a s o
   . (Unit -> StateValue) 
  -> H.HalogenM HookState a s o Aff { state :: StateValue, id :: StateId }
initializeHook mkInitialState = Prelude.do
  { state } <- H.get

  let
    initialState = mkInitialState unit

    queue =
      { queue: Array.snoc state.queue initialState
      , index: 0
      , total: state.total + 1
      }

  H.modify_ _ { state = queue }
  Prelude.pure { state: initialState, id: StateId state.total }

stepHook 
  :: forall a s o
   . H.HalogenM HookState a s o Aff { state :: StateValue, id :: StateId }
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

unsafeGetState :: StateId -> Array StateValue -> StateValue
unsafeGetState (StateId index) array = unsafePartial (Array.unsafeIndex array index)

unsafeSetState :: StateId -> StateValue -> Array StateValue -> Array StateValue
unsafeSetState (StateId index) a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))