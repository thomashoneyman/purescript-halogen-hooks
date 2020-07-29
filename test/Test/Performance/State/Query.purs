module Test.Performance.State.Query where

import Prelude

data Query a = Run (Unit -> a)
