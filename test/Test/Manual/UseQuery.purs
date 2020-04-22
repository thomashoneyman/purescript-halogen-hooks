module Test.Manual.UseQuery where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (Hooked, QueryToken, useQuery)
import Halogen.Hooks as Hooks

data Query a
  = PrintMessage String a
  | ReturnRandomInt (Int -> a)

sproxy :: SProxy "useQuery"
sproxy = SProxy

component :: forall o m. MonadEffect m => H.Component HH.HTML Query Unit o m
component = Hooks.componentWithQuery \tQuery _ -> hook tQuery

hook :: forall slots output m hooks
      . MonadEffect m
     => QueryToken Query
     -> Hooked slots output m hooks (Hooks.UseQuery hooks) _
hook tQuery = Hooks.do
  useQuery tQuery case _ of
    PrintMessage message next -> do
      liftEffect $ log $ "useQuery - PrintMessage: " <> message
      pure $ Just next
    ReturnRandomInt reply -> do
      liftEffect $ log $ "useQuery - ReturnRandomInt: generating random int"
      i <- liftEffect $ randomInt 0 10
      pure $ Just (reply i)

  Hooks.pure $
    HH.p_
      [ HH.text $ "This component can be queried by the parent component \
                  \to print a message or get a random integer."
      ]
