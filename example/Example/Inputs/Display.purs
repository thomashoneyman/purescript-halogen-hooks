  
module Example.Components.Inputs.Display where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hook as Hook

type Slot p = forall q. H.Slot q Void p

type Input = Int

component :: forall q. H.Component HH.HTML q Input Void Aff 
component = Hook.component \i -> Hook.do
  Hook.pure do
    HH.div_
      [ HH.text "My input value is:"
      , HH.strong_ [ HH.text (show i) ]
      ]