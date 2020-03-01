module Example.Hooks.Components where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.Lens (_Right, over)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Example.Hooks.UseLocalStorage (Key(..), useLocalStorage)
import Example.Hooks.UsePreviousValue (usePreviousValue)
import Example.Hooks.UseWindowWidth (useWindowWidth)
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

windowWidth :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
windowWidth = Hook.component \_ -> Hook.do
  width <- useWindowWidth
  Hook.pure do
    HH.div_
      [ HH.h4_ [ HH.text "Window Width" ]
      , HH.p_ [ HH.text "This example demonstrates a hook which subscribes to resize events on the window and returns its width on change." ]
      , HH.text $ "Current width: " <> maybe "" show width
      ]

previousValue :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
previousValue = Hook.component \_ -> Hook.do
  count /\ countState <- Hook.useState 0
  prevCount <- usePreviousValue count

  Hook.pure do
    HH.div
      [ ]
      [ HH.h4_ [ HH.text "Previous Value" ]
      , HH.p_ [ HH.text "This example demonstrates a hook to persist a value from the previous render." ]
      , HH.text $ "The previous value of the state 'count' was: " <> show prevCount
      , HH.br_
      , HH.button
          [ HE.onClick \_ -> Just (EH.modify_ countState (_ + 1)) ]
          [ HH.text $ "Increment (" <> show count <> ")" ]
      ]

localStorage :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
localStorage = Hook.component \_ -> Hook.do
  value /\ valueState <- useLocalStorage
    { defaultValue: 0
    , fromJson: decodeJson
    , toJson: encodeJson
    , key: Key "intStorageExample"
    }

  let
    clearCount =
      EH.put valueState (Right 0)

    increment =
      EH.modify_ valueState (over _Right (_ + 1))

  Hook.pure do
    HH.div
      [ ]
      [ HH.text "Click on the button to clear from local storage"
      , HH.button
          [ HE.onClick \_ -> Just clearCount ]
          [ HH.text "Clear" ]
      , HH.br_
      , HH.text $ "You have " <> either identity show value <> " at the intStorageExample key in local storage."
      , HH.button
          [ HE.onClick \_ -> Just increment ]
          [ HH.text "Increment" ]
      ]
