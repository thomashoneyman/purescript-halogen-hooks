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
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data HookF a
  = UseState Int (Unit -> StateValue) (StateInterface -> a)

derive instance functorHookF :: Functor HookF

type Hook = Free HookF

type HTML = H.ComponentHTML Action () Aff 

-- Effects that the user ought to be able to run in this component are
-- represented as actions with accompanying handlers.
data Action
  = ModifyState Int StateValue

handleAction
  :: forall output 
   . (HookF ~> H.HalogenM InternalState Action () output Aff)
  -> Action 
  -> H.HalogenM InternalState Action () output Aff Unit
handleAction interpretHook = case _ of
  ModifyState id newState -> do
    let unsafeUpdate arr = unsafePartial (fromJust (Array.modifyAt id (const newState) arr))
    { hook } <- H.modify \st -> st { state = unsafeUpdate st.state }
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

-- Underlying component

type InternalState =
  { html :: HTML
  , hook :: Hook HTML
  , state :: Array StateValue
  }

component :: forall q i o. Hook HTML -> H.Component HH.HTML q i o Aff
component hook =
  H.mkComponent
    { initialState: \_ -> 
        { html: HH.text ""
        , state: []
        , hook 
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
          handleAction (interpretHook false) act *> pure a

        Receive _ a -> 
          pure a

        Finalize a -> 
          pure a
    }
  where
  interpretHook :: Boolean -> HookF ~> H.HalogenM InternalState Action () o Aff
  interpretHook isInitial = case _ of
    UseState id initial' reply -> do
      if isInitial then do
        let 
          initial = initial' unit
          unsafeInsert arr = unsafePartial (fromJust (Array.insertAt id initial arr))
        st <- H.modify \st -> st { state = unsafeInsert st.state }
        pure (reply { get: initial, set: ModifyState id })
      
      else do
        let unsafeGet arr = unsafePartial (fromJust (Array.index arr id))
        { state } <- H.get
        pure (reply { get: unsafeGet state, set: ModifyState id })