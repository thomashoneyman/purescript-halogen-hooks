module Test.Shared where

import Prelude

import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Halogen (defaultEval, liftEffect, mkComponent, mkEval)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
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

_child :: SProxy "child"
_child = SProxy

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
        void $ H.query _child unit $ H.tell $
          UseQuery.PrintMessage "Parent message passed to child"
      QueryComponentViaRequest -> do
        mbInt <- H.query _child unit $ H.request UseQuery.ReturnRandomInt
        for_ mbInt \i -> do
          liftEffect $ log $
            "Parent: random int returned from child was " <> show i

    intercalateWithBr_ =
      foldl (\acc next -> acc <> [ HH.br_, next ]) []

    render { showLifecycleEffect } =
      HH.div_
        [ HH.button
          [ HE.onClick \_ -> Just ToggleLifecycleEffectRendering
          ]
          [ HH.text $ "Toggle rendering of component. Current state: " <>
              (if showLifecycleEffect then "rendering component" else "NOT rendering component")
          ]
        , HH.br_
        , if showLifecycleEffect
            then
              HH.div_
                [ HH.button
                  [ HE.onClick \_ -> Just QueryComponentViaTell ]
                  [ HH.text $ "Send a query to the component to cause it \
                              \to print a messaage to the console."
                  ]
                , HH.br_
                , HH.button
                  [ HE.onClick \_ -> Just QueryComponentViaRequest ]
                  [ HH.text $ "Send a query to the component to cause it \
                              \to return a random integer to the parent."
                  ]
                , HH.br_
                , HH.slot _child unit allHooks unit (const Nothing)
                ]
            else HH.text ""
        ]

    allHooks :: H.Component HH.HTML UseQuery.Query Unit _ _
    allHooks = Hooks.componentWithQuery \tQuery _ -> Hooks.do
      stateHtml <- UseState.hook
      _ <- UseLifecycleEffect.hook
      _ <- UseTickEffect.hook
      refHtml <- UseRef.hook
      memoHtml <- UseMemo.hook
      _ <- UseQuery.hook tQuery

      Hooks.pure $
        HH.div_ $ intercalateWithBr_
          [ HH.div_
            [ HH.h2_ [ HH.text "useState" ]
            , stateHtml
            ]

          , HH.div_
            [ HH.h2_ [ HH.text "useRef" ]
            , refHtml
            ]

          , HH.div_
            [ HH.h2_ [ HH.text "useMemo" ]
            , memoHtml
            ]
          ]
