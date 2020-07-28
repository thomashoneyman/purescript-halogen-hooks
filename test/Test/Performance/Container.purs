module Test.Performance.Container where

import Prelude

import Data.Array (concatMap, fold, replicate)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff.Util as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  _ <- runUI container unit body
  log "application started"

statePrefix = "state" :: String

tests :: Array Test
tests =
  [ { testPrefix: statePrefix, testType: StateTest, testTarget: Hook }
  , { testPrefix: statePrefix, testType: StateTest, testTarget: Component }
  ]

type Test =
  { testType :: TestType
  , testPrefix :: String
  , testTarget :: TestTarget
  }

data TestTarget = Hook | Component
derive instance eqTestTarget :: Eq TestTarget

toStringTestTarget :: TestTarget -> String
toStringTestTarget = case _ of
  Hook -> "hook"
  Component -> "component"

data TestType = StateTest
derive instance eqTestType :: Eq TestType

data Action = RunTest Test

container :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
container = H.mkComponent
  { initialState: \_ -> Nothing
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render mbTest =
    HH.div_
      [ HH.div [ HP.class_ $ HH.ClassName "tests" ] $
          tests <#> \test@{ testPrefix, testTarget } -> do
            let id = fold [ testPrefix, "-", toStringTestTarget testTarget ]
            HH.button
              [ HP.class_ $ HH.ClassName id
              , HE.onClick \_ -> Just (RunTest test)
              ]
              [ HH.text $ "Run " <> id ]
      , case mbTest of
          Nothing ->
            HH.text ""
          Just { testType, testTarget } -> case testType, testTarget of
            StateTest, Hook ->
              HH.slot (SProxy :: _ "aHook") unit testHook unit absurd

            StateTest, Component ->
              HH.slot (SProxy :: _ "aComponent") unit testComponent unit absurd
      ]

  handleAction = case _ of
    RunTest test -> H.put (Just test)

testComponent :: forall q i o m. H.Component HH.HTML q i o m
testComponent =
  H.mkComponent
    { initialState:
        \_ -> { n: 0, n1: 0, n2: 0, n3: 0, n4: 0, done: false }
    , render:
        \{ done } ->
          if done then
            HH.div [ HP.class_ $ HH.ClassName $ fold [ statePrefix, "-component", "-complete" ] ] [ ]
          else
            HH.text ""
    , eval:
        H.mkEval $ H.defaultEval { initialize = Just unit, handleAction = handleAction }
    }
  where
  handleAction _ = do
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n = s.n + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n1 = s.n1 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n2 = s.n2 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n3 = s.n3 + 1 }
    sequence_ $ replicate 50 $ H.modify_ \s -> s { n4 = s.n4 + 1 }
    H.modify_ _ { done = true }

testHook :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
testHook = Hooks.memoComponent (\_ _ -> false) \_ _ -> Hooks.do
  n /\ nId <- Hooks.useState 0
  n1 /\ n1Id <- Hooks.useState 0
  n2 /\ n2Id <- Hooks.useState 0
  n3 /\ n3Id <- Hooks.useState 0
  n4 /\ n4Id <- Hooks.useState 0
  done /\ doneId <- Hooks.useState false

  Hooks.useLifecycleEffect do
    sequence_ $ concatMap (\id -> replicate 50 (Hooks.modify_ id (_ + 1))) [ nId, n1Id, n2Id, n3Id, n4Id ]
    Hooks.modify_ doneId not
    pure Nothing

  Hooks.pure do
    if done then
      HH.div [ HP.class_ $ HH.ClassName $ fold [ statePrefix, "-hook", "-complete" ] ] [ ]
    else
      HH.text ""
