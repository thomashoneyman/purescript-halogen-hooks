module Halogen.Hook where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Data.Array as Array
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM (imapState, mapAction)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data HookF a
  = UseState Int (Unit -> StateValue) (StateInterface -> a)
  | UseAction Int (Unit -> StateValue) ActionHandler (Tuple StateValue (ActionValue -> Action) -> a)

derive instance functorHookF :: Functor HookF

type Hook = Free HookF

type HTML = H.ComponentHTML Action () Aff 

-- Effects that the user ought to be able to run in this component are
-- represented as actions with accompanying handlers.
data Action
  = ModifyState Int StateValue
  | RunAction Int ActionHandler ActionValue

handleAction
  :: Hook HTML
  -> (HookF ~> H.HalogenM InternalState Action () Void Aff)
  -> Action 
  -> H.HalogenM InternalState Action () Void Aff Unit
handleAction hook interpretHook = case _ of
  ModifyState id newState -> do
    let unsafeUpdate arr = unsafePartial (fromJust (Array.modifyAt id (const newState) arr))
    H.modify_ \st -> st { state = unsafeUpdate st.state }

    html <- foldFree interpretHook hook
    H.modify_ _ { html = html }

  RunAction id handler action -> do
    current <- H.get
    let 
      toState sv =
        current { state = unsafePartial $ fromJust $ Array.modifyAt id (const sv) current.state }
      fromState = \state -> unsafePartial $ fromJust $ Array.index state.state id

    -- Run the action
    (mapAction (RunAction id handler)) (imapState toState fromState (handler action))

    html <- foldFree interpretHook hook
    H.modify_ _ { html = html }

--  State

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

useState :: forall state. Int -> (Unit -> state) -> Hook (State state)
useState id initialState = liftF (UseState id initialStateValue hookInterface)
  where
  initialStateValue :: Unit -> StateValue
  initialStateValue = map toStateValue initialState

  hookInterface :: StateInterface -> State state
  hookInterface { get, set } = Tuple (fromStateValue get) (set <<< toStateValue) 

-- Action

foreign import data ActionValue :: Type

toActionValue :: forall action. action -> ActionValue
toActionValue = unsafeCoerce

fromActionValue :: forall action. ActionValue -> action
fromActionValue = unsafeCoerce

type ActionHandler = ActionValue -> H.HalogenM StateValue ActionValue () Void Aff Unit

useAction 
  :: forall state action
   . Int 
  -> (Unit -> state) 
  -> (action -> H.HalogenM state action () Void Aff Unit)
  -> Hook (Tuple state (action -> Action))
useAction id initialState handler = liftF (UseAction id initialStateValue hookHandler hookInterface)
  where
  initialStateValue :: Unit -> StateValue
  initialStateValue = map toStateValue initialState

  hookHandler :: ActionValue -> H.HalogenM StateValue ActionValue () Void Aff Unit
  hookHandler actionValue = do
    let 
      handler' :: H.HalogenM StateValue action () Void Aff Unit 
      handler' = imapState toStateValue fromStateValue (handler (fromActionValue actionValue))

    mapAction toActionValue handler'

  hookInterface :: Tuple StateValue (ActionValue -> Action) -> Tuple state (action -> Action)
  hookInterface (Tuple st fn) = Tuple (fromStateValue st) (fn <<< toActionValue)

-- Underlying component

type InternalState =
  { html :: HTML
  , state :: Array StateValue
  }

component :: forall q i. Hook HTML -> H.Component HH.HTML q i Void Aff
component hook =
  H.mkComponent
    { initialState: \_ -> 
        { html: HH.text ""
        , state: []
        }
    , render: _.html
    , eval: case _ of
        Initialize a -> do
          html <- foldFree (interpretHook true) hook
          H.modify_ _ { html = html }
          pure a

        Query _ reply -> 
          pure (reply unit)

        Action act a -> 
          handleAction hook (interpretHook false) act *> pure a

        Receive _ a -> 
          pure a

        Finalize a -> 
          pure a
    }
  where
  interpretHook :: Boolean -> HookF ~> H.HalogenM InternalState Action () Void Aff
  interpretHook isInitial = case _ of
    UseState id initial' reply -> do
      if isInitial then do
        let 
          initial = initial' unit
          unsafeInsert arr = unsafePartial (fromJust (Array.insertAt id initial arr))
        H.modify_ \st -> st { state = unsafeInsert st.state }
        pure (reply { get: initial, set: ModifyState id })
      
      else do
        let unsafeGet arr = unsafePartial (fromJust (Array.index arr id))
        { state } <- H.get
        pure (reply { get: unsafeGet state, set: ModifyState id })

    UseAction id initial' handler reply -> do
      if isInitial then do
        let 
          initial = initial' unit
          unsafeInsert arr = unsafePartial (fromJust (Array.insertAt id initial arr))
        H.modify_ \st -> st { state = unsafeInsert st.state }
        pure (reply $ Tuple initial (RunAction id handler))

      else do
        let unsafeGet arr = unsafePartial (fromJust (Array.index arr id))
        { state } <- H.get
        pure (reply $ Tuple (unsafeGet state) (RunAction id handler))