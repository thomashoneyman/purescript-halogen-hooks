module Example.Components.Inputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

data Action
  = Increment
  | Decrement
  | HandleChild Display.Output

type State = Int

type ChildSlots = ( display :: Display.Slot Int )

_display = SProxy :: SProxy "display"

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hook.component \_ -> Hook.do
  state /\ runAction <- Hook.useEval 1 (Hook.defaultEval { handleAction = handleAction })

  let handleChild = Just <<< runAction <<< HandleChild

  Hook.pure do
    HH.div_
      [ HH.ul_
          [ HH.slot _display 1 Display.component state handleChild
          , HH.slot _display 2 Display.component (state * 2) handleChild
          , HH.slot _display 3 Display.component (state * 3) handleChild
          , HH.slot _display 4 Display.component (state * 10) handleChild
          , HH.slot _display 5 Display.component (state * state) handleChild
          ]
      , HH.button
          [ HE.onClick \_ -> Just (runAction Decrement) ]
          [ HH.text "+ 1"]
      , HH.button
          [ HE.onClick \_ -> Just (runAction Increment) ]
          [ HH.text "+ 1"]
      ]

handleAction :: forall out m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots out m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ (_ - 1)

  Increment ->
    H.modify_ (_ + 1)

  HandleChild i ->
    H.liftEffect $ Console.logShow i
