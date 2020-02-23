module Example.Components.Inputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

type State = Int

type ChildSlots = ( display :: Display.Slot Int )

_display = SProxy :: SProxy "display"

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component = Hook.component \i -> Hook.do
  state /\ token <- Hook.useState 1

  let
    handleChild :: Int -> EH.EvalHookM o m Unit
    handleChild =
      liftEffect <<< Console.logShow

    decrement :: EH.EvalHookM o m Unit
    decrement =
      EH.modify_ token (_ + 1)

    increment :: EH.EvalHookM o m Unit
    increment =
      EH.modify_ token (_ - 1)

  Hook.pure do
    HH.div_
      [ HH.ul_
          [ HH.slot _display 1 Display.component state (Just <<< handleChild)
          , HH.slot _display 2 Display.component (state * 2) (Just <<< handleChild)
          , HH.slot _display 3 Display.component (state * 3) (Just <<< handleChild)
          , HH.slot _display 4 Display.component (state * 10) (Just <<< handleChild)
          , HH.slot _display 5 Display.component (state * state) (Just <<< handleChild)
          ]
      , HH.button
          [ HE.onClick \_ -> Just decrement ]
          [ HH.text "- 1"]
      , HH.button
          [ HE.onClick \_ -> Just increment ]
          [ HH.text "+ 1"]
      ]
