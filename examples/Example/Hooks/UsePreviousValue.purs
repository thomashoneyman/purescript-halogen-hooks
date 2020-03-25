module Example.Hooks.UsePreviousValue
  ( usePreviousValue
  , UsePreviousValue
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (Hook, UseEffect, UseRef)
import Halogen.Hooks as Hooks

type UsePreviousValue' a hooks = UseEffect (UseRef (Maybe a) hooks)

foreign import data UsePreviousValue :: Type -> Type -> Type

usePreviousValue
  :: forall slots output m a
   . MonadAff m
  => Eq a
  => a
  -> Hook slots output m (UsePreviousValue a) (Maybe a)
usePreviousValue value = Hooks.publish hook
  where
  hook :: Hook slots output m (UsePreviousValue' a) (Maybe a)
  hook = Hooks.do
    prev /\ ref <- Hooks.useRef Nothing

    Hooks.captures { } Hooks.useTickEffect do
      liftEffect $ Ref.write (Just value) ref
      pure Nothing

    Hooks.pure prev
