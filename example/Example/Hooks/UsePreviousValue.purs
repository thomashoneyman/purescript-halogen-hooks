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
import Halogen.Hook (Hook, UseEffect, UseRef)
import Halogen.Hook as Hook

type UsePreviousValue' a hooks = UseEffect (UseRef (Maybe a) hooks)

foreign import data UsePreviousValue :: Type -> Type -> Type

usePreviousValue
  :: forall ps o m a
   . MonadAff m
  => Eq a
  => a
  -> Hook ps o m (UsePreviousValue a) (Maybe a)
usePreviousValue value = Hook.coerce hook
  where
  hook :: Hook ps o m (UsePreviousValue' a) (Maybe a)
  hook = Hook.do
    prev /\ ref <- Hook.useRef Nothing

    Hook.captures { } Hook.useTickEffect do
      liftEffect $ Ref.write (Just value) ref
      pure Nothing

    Hook.pure prev
