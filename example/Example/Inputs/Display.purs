module Example.Components.Inputs.Display where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Hook as Hook

type Input = Int

component :: forall q o m. H.Component HH.HTML q Input o m
component = Hook.component \input -> Hook.pure do
  HH.div_
    [ HH.text "My input value is: "
    , HH.strong_
        [ HH.text $ show input ]
    ]
