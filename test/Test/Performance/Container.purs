module Test.Performance.Container where

import Prelude

import Data.Array (all, concatMap, replicate)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  runUI container unit body

data TestState
  = NotStarted
  | HookTest
  | ComponentTest
  | Completed

derive instance eqTestState :: Eq TestState

type State =
  { a :: TestState }

data Action
  = RunHook TestHook

data TestHook
  = A

container :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
container = H.mkComponent
  { initialState: \_ -> { a: NotStarted }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render { a } =
    HH.div
      [ ]
      [ actions
      , whenElem (a == HookTest) \_ ->
          HH.slot (SProxy :: _ "aHook") unit testHook unit absurd
      , whenElem (a == ComponentTest) \_ ->
          HH.slot (SProxy :: _ "aComponent") unit testComponent unit absurd
      ]

  handleAction = case _ of
    RunHook A -> do
      H.gets _.a >>= case _ of
        NotStarted -> do
          H.modify_ _ { a = HookTest }

        HookTest -> do
          H.modify_ _ { a = ComponentTest }

        ComponentTest -> do
          H.modify_ _ { a = Completed }

        Completed -> do
          pure unit

actions :: forall w. HH.HTML w Action
actions =
  HH.div
    [ HP.id_ "actions" ]
    [ HH.button
        [ HP.id_ "a"
        , HE.onClick \_ -> Just $ RunHook A
        ]
        [ HH.text "Run Hook A" ]
    ]

whenElem :: forall w i. Boolean -> (Unit -> HH.HTML w i) -> HH.HTML w i
whenElem cond f = if cond then f unit else HH.text ""

testComponent :: forall q i o m. H.Component HH.HTML q i o m
testComponent = H.mkComponent
  { initialState: \_ -> { n: 0, n1: 0, n2: 0, n3: 0, n4: 0 }
  , render: \{ n, n1, n2, n3, n4 } ->
      whenElem (all (_ >= 50) [ n, n1, n2, n3, n4 ]) \_ ->
        HH.div
          [ HP.id_ "completed"]
          [ HH.text (show n) ]
  , eval: H.mkEval $ H.defaultEval { initialize = Just unit, handleAction = handleAction }
  }
  where
  handleAction _ = do
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n = s.n + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n1 = s.n1 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n2 = s.n2 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n3 = s.n3 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n4 = s.n4 + 1 }

testHook :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
testHook = Hooks.memoComponent (\_ _ -> false) \_ _ -> Hooks.do
  n /\ nId <- Hooks.useState 0
  n1 /\ n1Id <- Hooks.useState 0
  n2 /\ n2Id <- Hooks.useState 0
  n3 /\ n3Id <- Hooks.useState 0
  n4 /\ n4Id <- Hooks.useState 0

  Hooks.useLifecycleEffect do
    sequence_ $ concatMap (\id -> replicate 50 (Hooks.modify_ id (_ + 1))) [ nId, n1Id, n2Id, n3Id, n4Id ]
    pure Nothing

  Hooks.pure do
    whenElem (all (_ >= 50) [ n, n1, n2, n3, n4 ]) \_ ->
      HH.div
        [ HP.id_ "completed"]
        [ HH.text (show n) ]

