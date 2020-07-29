module Test.Performance.Test where

import Prelude

data Test
  = StateHook
  | StateComponent
  | TodoHook

derive instance eqTest :: Eq Test
derive instance ordTest :: Ord Test

testToString :: Test -> String
testToString = case _ of
  StateHook -> "state-hook"
  StateComponent -> "state-component"
  TodoHook -> "todo-hook"

-- Used by a test along with its string id to control test start / stop
startSuffix = "-start" :: String
completedSuffix = "-complete" :: String
