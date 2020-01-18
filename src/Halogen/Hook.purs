module Halogen.Hook where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Partial.Unsafe (unsafePartial)

data HookF state a
  = UseState Int state ((Tuple state ((state -> state) -> Action)) -> a)

derive instance functorHookF :: Functor (HookF state)

type Hook state = Free (HookF state)

useState :: forall state. Int -> state -> Hook state (Tuple state ((state -> state) -> Action))
useState index initialState = liftF (UseState index initialState identity)

-- Temporary for testing purposes
type StateType = String

data Action
  = Initialize
  | HookAction HookAction

data HookAction
  = ModifyState Int (StateType -> StateType)

type State m =
  { html :: H.ComponentHTML Action () m
  , state :: Array StateType
  , hook :: Hook StateType (H.ComponentHTML Action () m)
  }

type Input m =
  { hook :: Hook StateType (H.ComponentHTML Action () m)
  }

hookComponent :: forall f o m. H.Component HH.HTML f (Input m) o m
hookComponent =
  H.mkComponent
    { initialState
    , render: _.html
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }
  where
  initialState :: Input m -> State m
  initialState input = { html: HH.text "", state: [], hook: input.hook }

  handleAction :: Action -> H.HalogenM (State m) _ () _ m Unit
  handleAction = case _ of
    Initialize -> do
      { hook } <- H.get
      html <- foldFree (interpretHook true) hook
      H.modify_ _ { html = html }

    HookAction (ModifyState ix f) -> do
      let unsafeModifyAt arr = unsafePartial (fromJust (Array.modifyAt ix f arr))
      H.modify_ \st -> st { state = unsafeModifyAt st.state }

      { hook } <- H.get
      html <- foldFree (interpretHook false) hook
      H.modify_ _ { html = html }

  interpretHook :: Boolean -> HookF StateType ~> H.HalogenM (State m) Action () o m
  interpretHook isInitializer = case _ of
    UseState index initial reply -> do
      if isInitializer then do
        let unsafeInsert arr = (unsafePartial (fromJust (Array.insertAt index initial arr)))
        { state } <- H.modify \st -> st { state = unsafeInsert st.state }
        pure (reply (Tuple initial (HookAction <<< ModifyState index)))
      else do
        let unsafeIndex arr = unsafePartial (fromJust (Array.index arr index))
        { state } <- H.get
        pure (reply (Tuple (unsafeIndex state) (HookAction <<< ModifyState index)))