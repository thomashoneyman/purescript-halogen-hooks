module Test.Performance.Todo.Component where

import Prelude

import Halogen as H
import Halogen.HTML as HH

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }
    , render: HH.text <<< show
    , eval: H.mkEval $ H.defaultEval
    }
