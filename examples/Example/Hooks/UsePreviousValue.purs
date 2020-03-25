module Example.Hooks.UsePreviousValue
  ( usePreviousValue
  , UsePreviousValue
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (Hook, UseEffect, UseRef)
import Halogen.Hooks as Hooks

newtype UsePreviousValue a hooks =
  UsePreviousValue (UseEffect (UseRef (Maybe a) hooks))

derive instance newtypeUsePreviousValue :: Newtype (UsePreviousValue a hooks) _

usePreviousValue
  :: forall slots output m a
   . MonadAff m
  => Eq a
  => a
  -> Hook slots output m (UsePreviousValue a) (Maybe a)
usePreviousValue value = Hooks.wrap Hooks.do
  prev /\ ref <- Hooks.useRef Nothing

  Hooks.captures { } Hooks.useTickEffect do
    liftEffect $ Ref.write (Just value) ref
    pure Nothing

  Hooks.pure prev
