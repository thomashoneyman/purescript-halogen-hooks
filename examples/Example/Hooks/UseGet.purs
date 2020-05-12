module Example.Hooks.UseGet
  ( useGet
  , UseGet
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, UseEffect, UseRef)
import Halogen.Hooks as Hooks

foreign import data UseGet :: Type -> Hooks.HookType

instance newtypeUseGet :: HookNewtype (UseGet a) (UseRef a <> UseEffect <> Hooks.Nil)

useGet :: forall m a. MonadEffect m => a -> Hook m (UseGet a) (HookM m a)
useGet latest = Hooks.wrap Hooks.do
  _ /\ ref <- Hooks.useRef latest

  Hooks.captures {} Hooks.useTickEffect do
    liftEffect $ Ref.write latest ref
    pure Nothing

  Hooks.pure (liftEffect $ Ref.read ref)
