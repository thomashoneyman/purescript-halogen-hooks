module Test.Setup.Performance.App where

import Prelude

import Data.Maybe (Maybe(..), isJust)
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
import Test.Performance.State.Query as State.Query

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  runUI container unit body

data TestState
  = NotStarted
  | Running Test
  | Completed Test

data Test
  = StateHook
  | StateComponent

testToString :: Test -> String
testToString = case _ of
  StateHook -> "state-hook"
  StateComponent -> "state-component"

testsId = "tests" :: String
completedSuffix = "-completed" :: String

container :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
container = H.mkComponent
  { initialState: \_ -> NotStarted
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  testAction test = do
    let test' = testToString test
    HH.button [ HP.id_ test', HE.onClick \_ -> Just test ] [ HH.text test' ]

  viewTest = case _ of
    -- indicates to Puppeteer that the test has completed
    Completed test -> HH.div [ HP.id_ (testToString test <> completedSuffix) ] [ ]
    -- indicates that Puppeteer has started the test
    _ -> HH.text ""

  render state =
    HH.div_
      [ HH.div -- where Puppeteer can hook in to trigger tests
          [ HP.id_ testsId ]
          [ testAction StateHook
          , HH.slot State.Hook._stateHook unit State.Hook.component unit absurd
          , testAction StateComponent
          , HH.slot State.Component._stateComponent unit State.Component.component unit absurd
          ]
      , viewTest state
      ]

  handleAction = case _ of
    test@StateHook -> do
      H.put (Running test)
      done <- H.query State.Hook._stateHook unit (State.Query.Run identity)
      when (isJust done) do
        H.put (Completed test)

    test@StateComponent -> do
      H.put (Running test)
      done <- H.query State.Component._stateComponent unit (State.Query.Run identity)
      when (isJust done) do
        H.put (Completed test)
