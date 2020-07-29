module Test.Performance.State.Hook where

import Prelude

import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Test.Performance.State.Query (Query(..))

type Slot = H.Slot Query Void

_stateHook = SProxy :: SProxy "stateHook"

component :: forall i o m. MonadAff m => H.Component HH.HTML Query i o m
component = Hooks.component \{ queryToken } _ -> Hooks.do
  n /\ nId <- Hooks.useState { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }

  Hooks.useQuery queryToken case _ of
    Run reply -> do
      sequence_ $ replicate 50 $ Hooks.modify_ nId \s -> s { n = s.n + 1 }
      sequence_ $ replicate 50 $ Hooks.modify_ nId \s -> s { n1 = s.n1 + 1 }
      sequence_ $ replicate 50 $ Hooks.modify_ nId \s -> s { n2 = s.n2 + 1 }
      sequence_ $ replicate 50 $ Hooks.modify_ nId \s -> s { n3 = s.n3 + 1 }
      sequence_ $ replicate 50 $ Hooks.modify_ nId \s -> s { n4 = s.n4 + 1 }
      pure (Just (reply unit))

  Hooks.pure do
    HH.text $ show n
