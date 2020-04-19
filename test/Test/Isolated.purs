module Test.Isolated where

import Prelude

import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Halogen (defaultEval, liftEffect, mkComponent, mkEval)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Test.UseLifecycleEffect as UseLifecycleEffect
import Test.UseMemo as UseMemo
import Test.UseQuery as UseQuery
import Test.UseRef as UseRef
import Test.UseState as UseState
import Test.UseTickEffect as UseTickEffect

data Action
  = ToggleLifecycleEffectRendering
  | QueryComponentViaTell
  | QueryComponentViaRequest

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI rootComponent unit body
  where
    rootComponent =
      mkComponent
        { initialState: \_ ->
            { showLifecycleEffect: true -- whether to render lifecycle component or not
            }
        , render
        , eval: mkEval $ defaultEval { handleAction = handleAction }
        }

    handleAction = case _ of
      ToggleLifecycleEffectRendering -> do
        H.modify_ (\s -> s { showLifecycleEffect = not s.showLifecycleEffect })
      QueryComponentViaTell -> do
        void $ H.query UseQuery.sproxy unit $ H.tell $
          UseQuery.PrintMessage "Parent message passed to child"
      QueryComponentViaRequest -> do
        mbInt <- H.query UseQuery.sproxy unit $ H.request UseQuery.ReturnRandomInt
        for_ mbInt \i -> do
          liftEffect $ log $
            "Parent: random int returned from child was " <> show i

    render st =
      HH.div_ $ intercalateWithBr_ (itemsToRender st)

    intercalateWithBr_ =
      foldl (\acc next -> acc <> [ HH.br_, next ]) []

    itemsToRender { showLifecycleEffect } =
      [ HH.div_
        [ HH.h2_ [ HH.text "useState" ]
        , HH.slot UseState.sproxy unit UseState.component unit (const Nothing)
        ]

      , HH.div_
        [ HH.h2_ [ HH.text "useLifecycleEffect" ]
        , HH.button
          [ HE.onClick \_ -> Just ToggleLifecycleEffectRendering
          ]
          [ HH.text $ "Toggle render of component. Current state: " <>
              (if showLifecycleEffect then "rendering component" else "NOT rendering component")
          ]
        , HH.br_
        , if showLifecycleEffect
          then HH.slot UseLifecycleEffect.sproxy unit UseLifecycleEffect.component unit (const Nothing)
          else HH.text ""
        ]

      , HH.div_
        [ HH.h2_ [ HH.text "useTickEffect" ]
        , HH.slot UseTickEffect.sproxy unit UseTickEffect.component unit (const Nothing)
        ]

      , HH.div_
        [ HH.h2_ [ HH.text "useRef" ]
        , HH.slot UseRef.sproxy unit UseRef.component unit (const Nothing)
        ]

      , HH.div_
        [ HH.h2_ [ HH.text "useMemo" ]
        , HH.slot UseMemo.sproxy unit UseMemo.component unit (const Nothing)
        ]

      , HH.div_
        [ HH.h2_ [ HH.text "useQuery" ]
        , HH.button
          [ HE.onClick \_ -> Just QueryComponentViaTell ]
          [ HH.text $ "Click me to use a query on the component to cause it \
                      \to print a messaage to the console."
          ]
        , HH.br_
        , HH.button
          [ HE.onClick \_ -> Just QueryComponentViaRequest ]
          [ HH.text $ "Click me to use a query on the component to cause it \
                      \to return to the parent a random integer."
          ]
        , HH.slot UseQuery.sproxy unit UseQuery.component unit (const Nothing)
        ]
      ]
