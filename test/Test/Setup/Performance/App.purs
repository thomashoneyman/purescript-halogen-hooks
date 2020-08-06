module Test.Setup.Performance.App where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff.Util as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Test.Performance.State.Component as State.Component
import Test.Performance.State.Hook as State.Hook
import Test.Performance.Test (Test(..), completedSuffix, testToString)
import Test.Performance.Todo.Component as Todo.Component
import Test.Performance.Todo.Hook as Todo.Hook

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  runUI container unit body

data TestState
  = NotStarted
  | Running Test
  | Completed Test

derive instance eqTestState :: Eq TestState

data Action = HandleStartTest Test | HandleTestComplete Test

container :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
container = H.mkComponent
  { initialState: \_ -> NotStarted
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  -- Used by Puppeteer to mount a test into the page so that it can be started
  testAction test = do
    let test' = testToString test
    HH.button [ HP.id_ test', HE.onClick \_ -> Just (HandleStartTest test) ] [ HH.text test' ]

  handleComplete test =
    Just <<< const (HandleTestComplete test)

  render state = do
    HH.div_
      [ HH.div_
          [ -- Used by Puppeteer to trigger a test to be mounted into the page
            testAction StateHook
          , testAction StateComponent
          , testAction TodoHook
          , testAction TodoComponent

          , case state of
              NotStarted ->
                HH.text ""

              Running StateHook ->
                HH.slot State.Hook._stateHook unit State.Hook.component unit (handleComplete StateHook)

              Running StateComponent ->
                HH.slot State.Component._stateComponent unit State.Component.component unit (handleComplete StateComponent)

              Running TodoHook ->
                HH.slot Todo.Hook._todoHook unit Todo.Hook.container unit (handleComplete TodoHook)

              Running TodoComponent ->
                HH.slot Todo.Component._todoComponent unit Todo.Component.container unit (handleComplete TodoComponent)

              Completed test ->
                HH.div [ HP.id_ (testToString test <> completedSuffix) ] [ ]
          ]
      ]

  handleAction = case _ of
    HandleStartTest test ->
      H.put (Running test)

    HandleTestComplete test ->
      H.put (Completed test)
