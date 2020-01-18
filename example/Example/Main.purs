module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook (Hook)
import Halogen.Hook as Hook
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Hook.hookComponent { hook: testHook } body

testHook :: forall m. Hook String (H.ComponentHTML Hook.Action () m)
testHook = do
  Tuple helloState modifyHello <- Hook.useState 0 "Hello"
  Tuple worldState modifyWorld <- Hook.useState 1 "World"

  pure $
    HH.button
      [ HE.onClick \_ -> Just $ modifyWorld (_ <> "!") ]
      [ HH.text (helloState <> " " <> worldState) ]
