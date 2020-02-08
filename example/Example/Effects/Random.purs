module Example.Effects.Random where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hook as Hook

data Action = Regenerate 

handleAction :: Action -> H.HalogenM (Maybe Number) Action () Void Aff Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)

component :: forall f i. H.Component HH.HTML f i Void Aff
component = Hook.component Hook.do
  state /\ action <- Hook.useAction handleAction \_ -> Nothing

  let value = maybe "No number generated yet" show state

  Hook.pure $ HH.div_
    [ HH.h1_ 
        [ HH.text "Random number" ]
    , HH.p_ 
        [ HH.text $ "Current value: " <> value ]
    , HH.button
        [ HE.onClick \_ -> Just $ action Regenerate ]
        [ HH.text "Generate new number" ]
    ]
