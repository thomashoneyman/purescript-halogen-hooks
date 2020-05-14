module Example.Hooks.UsePrevious
  ( usePrevious
  , UsePrevious
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (class HookNewtype, type (<>), Hook, UseEffect, UseRef)
import Halogen.Hooks as Hooks

foreign import data UsePrevious :: Type -> Hooks.HookType

instance newtypeUsePrevious
  :: HookNewtype (UsePrevious a) (UseRef (Maybe a) <> UseEffect <> Hooks.Nil)

usePrevious :: forall m a. MonadAff m => Eq a => a -> Hook m (UsePrevious a) (Maybe a)
usePrevious value = Hooks.wrap Hooks.do
  prev /\ ref <- Hooks.useRef Nothing

  Hooks.captures { } Hooks.useTickEffect do
    liftEffect $ Ref.write (Just value) ref
    pure Nothing

  Hooks.pure prev
