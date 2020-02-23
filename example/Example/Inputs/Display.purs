module Example.Components.Inputs.Display where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

type Slot p = forall q. H.Slot q Output p

type Output = Int

type Input = Int

component :: forall q m. Functor m => H.Component HH.HTML q Input Output m
component = Hook.component \i -> Hook.pure do
  HH.div_
    [ HH.text "My input value is: "
    , HH.strong
        [ HE.onClick \_ -> Just (EH.raise i) ]
        [ HH.text (show i) ]
    ]
