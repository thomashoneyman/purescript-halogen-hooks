module Performance.Test.State.Component where

import Prelude

import Data.Array.NonEmpty (replicate)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Performance.Test.State.Shared (Output(..), stateUpdates)
import Performance.Test.Types (Test(..), startSuffix, testToString)

_stateComponent = SProxy :: SProxy "stateComponent"

data Action = RunState

component :: forall q i m. H.Component HH.HTML q i Output m
component =
  H.mkComponent
    { initialState: \_ -> { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render state =
    HH.div_
      [ HH.button
          [ HP.id_ (testToString StateComponent <> startSuffix)
          , HE.onClick \_ -> Just RunState
          ]
          [ HH.text "Start Test" ]
      , HH.text $ show state
      ]

  handleAction RunState = do
    sequence_ $ replicate stateUpdates $ H.modify_ \s -> s { n = s.n + 1 }
    sequence_ $ replicate stateUpdates $ H.modify_ \s -> s { n1 = s.n1 + 1 }
    sequence_ $ replicate stateUpdates $ H.modify_ \s -> s { n2 = s.n2 + 1 }
    sequence_ $ replicate stateUpdates $ H.modify_ \s -> s { n3 = s.n3 + 1 }
    sequence_ $ replicate stateUpdates $ H.modify_ \s -> s { n4 = s.n4 + 1 }
    H.raise Done
