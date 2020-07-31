module Example.Hooks.Components where

import Prelude

import Data.Argonaut (decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..), either)
import Data.Lens (_Right, over)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Example.Hooks.UseDebouncer (useDebouncer)
import Example.Hooks.UseLocalStorage (Key(..), useLocalStorage)
import Example.Hooks.UsePreviousValue (usePreviousValue)
import Example.Hooks.UseStateFn (useStateFn)
import Example.Hooks.UseWindowWidth (useWindowWidth)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks

windowWidth :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
windowWidth = Hooks.component \_ _ -> Hooks.do
  width <- useWindowWidth
  Hooks.pure do
    HH.div_
      [ HH.h4_ [ HH.text "Window Width" ]
      , HH.p_ [ HH.text "This example demonstrates a hook which subscribes to resize events on the window and returns its width on change." ]
      , HH.text $ "Current width: " <> maybe "" show width
      ]

previousValue :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
previousValue = Hooks.component \_ _ -> Hooks.do
  state /\ modifyState <- useStateFn Hooks.modify_ 0
  prevState <- usePreviousValue state

  Hooks.pure do
    HH.div
      [ ]
      [ HH.h4_ [ HH.text "Previous Value" ]
      , HH.p_ [ HH.text "This example demonstrates a hook to persist a value from the previous render." ]
      , HH.text $ "The previous value of the state 'count' was: " <> show prevState
      , HH.br_
      , HH.button
          [ HE.onClick \_ -> Just (modifyState (_ + 1)) ]
          [ HH.text $ "Increment (" <> show state <> ")" ]
      ]

localStorage :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
localStorage = Hooks.component \_ _ -> Hooks.do
  state /\ modifyState <- useLocalStorage
    { defaultValue: 0
    , fromJson: decodeJson
    , toJson: encodeJson
    , key: Key "intStorageExample"
    }

  let
    clearCount =
      modifyState \_ -> Right 0

    increment =
      modifyState (over _Right (_ + 1))

  Hooks.pure do
    HH.div
      [ ]
      [ HH.text "Click on the button to clear from local storage"
      , HH.button
          [ HE.onClick \_ -> Just clearCount ]
          [ HH.text "Clear" ]
      , HH.br_
      , HH.text $ "You have " <> either printJsonDecodeError show state <> " at the intStorageExample key in local storage."
      , HH.button
          [ HE.onClick \_ -> Just increment ]
          [ HH.text "Increment" ]
      ]

debouncer :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
debouncer = Hooks.component \_ _ -> Hooks.do
  text /\ setText <- useStateFn Hooks.put ""
  debouncedText /\ setDebouncedText <- useStateFn Hooks.put ""

  debouncedHandleInput <-
    useDebouncer (Milliseconds 300.0) setDebouncedText

  let
    handleInput str = Just do
      setText str
      debouncedHandleInput str

  Hooks.pure do
    HH.div_
      [ HH.h4_
          [ HH.text "Debounced Input" ]
      , HH.p_
          [ HH.text "This hook demonstrates debouncing an effectful function." ]
      , HH.input
          [ HE.onValueInput handleInput ]
      , HH.p_
          [ HH.text $ "You entered: " <> text ]
      , HH.p_
          [ HH.text $ "You entered (debounced): " <> debouncedText ]
      ]
