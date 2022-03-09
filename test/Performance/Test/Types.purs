module Performance.Test.Types where

import Prelude

data Test
  = StateHook
  | StateComponent
  | TodoHook
  | TodoComponent

derive instance Eq Test
derive instance Ord Test

testToString :: Test -> String
testToString = case _ of
  StateHook -> "state-hook"
  StateComponent -> "state-component"
  TodoHook -> "todo-hook"
  TodoComponent -> "todo-component"

-- Used by a test along with its string id to control test start / stop
startSuffix = "-start" :: String
completedSuffix = "-complete" :: String
