module Example.Effects.Random where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.EvalHookM as EH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

data Action = Regenerate

type State = Maybe Number

handleAction :: forall out m. MonadEffect m => EH.StateToken State -> Action -> EH.EvalHookM out m Unit
handleAction token = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    EH.put token (Just newNumber)

component :: forall f i o m. MonadEffect m => H.Component HH.HTML f i o m
component = Hook.component \_ -> Hook.do
  state /\ token <- Hook.useState Nothing

  let value = maybe "No number generated yet" show state

  Hook.pure do
    HH.div_
      [ HH.h1_
          [ HH.text "Random number" ]
      , HH.p_
          [ HH.text $ "Current value: " <> value ]
      , HH.button
          [ HE.onClick \_ -> Just $ handleAction token Regenerate ]
          [ HH.text "Generate new number" ]
      ]
