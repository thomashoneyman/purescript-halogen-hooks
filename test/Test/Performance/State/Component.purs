module Test.Performance.State.Component where

import Prelude

import Data.Array.NonEmpty (replicate)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Test.Performance.State.Query (Query(..))

type Slot = H.Slot Query Void

_stateComponent = SProxy :: SProxy "stateComponent"

component :: forall i o m. H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState: \_ -> { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }
    , render: HH.text <<< show
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where
  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (Run reply) = do
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n = s.n + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n1 = s.n1 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n2 = s.n2 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n3 = s.n3 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n4 = s.n4 + 1 }
    pure (Just (reply unit))
