module Test.Performance.Container where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Array (concatMap, replicate)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff.Util as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

foreign import setInterface :: forall a. (String -> Effect (Promise (Maybe Unit))) -> Effect Unit

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  io <- runUI container unit body

  let
    q' :: String -> Effect (Promise (Maybe Unit))
    q' = case _ of
      _ -> fromAff (io.query (RunHook identity))

  liftEffect do
    setInterface q'

data Query a = RunHook (Unit -> a) | RunComponent (Unit -> a)

data EnabledTest = None | TestA | TestB

container :: forall i o m. MonadAff m => H.Component HH.HTML Query i o m
container = H.mkComponent
  { initialState: \_ -> None
  , render
  , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
  }
  where
  render = case _ of
    None -> HH.p [ HP.id_ "z" ] [ HH.text "" ]
    TestA -> HH.slot (SProxy :: _ "aHook") unit testHook unit absurd
    TestB -> HH.slot (SProxy :: _ "aComponent") unit testComponent unit absurd

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    RunHook reply -> do
      H.put TestA
      _ <- H.query (SProxy :: _ "aHook") unit (Run identity)
      pure (Just (reply unit))

    RunComponent reply -> do
      H.put TestB
      _ <- H.query (SProxy :: _ "aComponent") unit (Run identity)
      pure (Just (reply unit))

data Run a = Run (Unit -> a)

testComponent :: forall i o m. H.Component HH.HTML Run i o m
testComponent =
  H.mkComponent
    { initialState:
        \_ -> { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }
    , render:
        \{ n, n1, n2, n3, n4 } ->
          HH.div_ $ map (HH.text <<< show) [ n, n1, n2, n3, n4 ]
    , eval:
        H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where
  handleQuery :: forall a. Run a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery (Run reply) = do
    void $ H.fork do
      sequence_ $ replicate 50 $ H.modify_ \s -> s { n = s.n + 1 }

    void $ H.fork do
      sequence_ $ replicate 50 $ H.modify_ \s -> s { n1 = s.n1 + 1 }

    void $ H.fork do
      sequence_ $ replicate 50 $ H.modify_ \s -> s { n2 = s.n2 + 1 }

    void $ H.fork do
      sequence_ $ replicate 50 $ H.modify_ \s -> s { n3 = s.n3 + 1 }

    void $ H.fork do
      sequence_ $ replicate 50 $ H.modify_ \s -> s { n4 = s.n4 + 1 }

    pure (Just (reply unit))

testHook :: forall i o m. MonadAff m => H.Component HH.HTML Run i o m
testHook = Hooks.memoComponent (\_ _ -> false) \{ queryToken } _ -> Hooks.do
  n /\ nId <- Hooks.useState 0
  n1 /\ n1Id <- Hooks.useState 0
  n2 /\ n2Id <- Hooks.useState 0
  n3 /\ n3Id <- Hooks.useState 0
  n4 /\ n4Id <- Hooks.useState 0

  Hooks.useQuery queryToken case _ of
    Run reply -> do
      sequence_ $ concatMap (\id -> replicate 50 (Hooks.modify_ id (_ + 1))) [ nId, n1Id, n2Id, n3Id, n4Id ]
      pure (Just (reply unit))

  Hooks.pure do
    HH.div_ $ map (HH.text <<< show) [ n, n1, n2, n3, n4 ]

