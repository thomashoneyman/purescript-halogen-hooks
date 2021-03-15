module Example.Halogen.ComponentsInputs.Display where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

type Input = Int

component :: forall q o m. H.Component q Input o m
component = Hooks.component \_ input -> Hooks.pure do
  HH.div_
    [ HH.text "My input value is: "
    , HH.strong_ [ HH.text $ show input ]
    ]
