module Example.Effects.Random where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

data Action = Regenerate 

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM (Maybe Number) Action () o m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)

component :: forall f i o. H.Component HH.HTML f i o Aff
component = Hook.component do
  (state :: Maybe Number) /\ setState <- Hook.useState 0 \_ -> Nothing

  let value = maybe "No number generated yet" show state

  pure $ HH.div_
    [ HH.h1_ 
        [ HH.text "Random number" ]
    , HH.p_ 
        [ HH.text $ "Current value: " <> value ]
    , HH.button
        [ HE.onClick \_ -> Nothing ] -- Just Regenerate ]
        [ HH.text "Generate new number" ]
    ]
