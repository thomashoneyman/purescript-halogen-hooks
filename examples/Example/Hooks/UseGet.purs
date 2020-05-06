module Example.Hooks.UseGet
  ( useGet
  , UseGet
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (Hook, HookM, UseEffect, UseRef)
import Halogen.Hooks as Hooks

newtype UseGet a hooks = UseGet (UseEffect (UseRef a hooks))

derive instance newtypeUseGet :: Newtype (UseGet a hooks) _

useGet
  :: forall m a
   . MonadEffect m
  => a
  -> Hook m (UseGet a) (HookM m a)
useGet latest = Hooks.wrap Hooks.do
  _ /\ ref <- Hooks.useRef latest

  Hooks.captures {} Hooks.useTickEffect do
    liftEffect $ Ref.write latest ref
    pure Nothing

  Hooks.pure (liftEffect $ Ref.read ref)
