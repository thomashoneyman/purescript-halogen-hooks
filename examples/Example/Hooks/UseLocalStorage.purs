module Example.Hooks.UseLocalStorage
  ( useLocalStorage
  , UseLocalStorage
  , Key(..)
  , StorageInterface
  )
  where

import Prelude

import Data.Argonaut (Json, jsonParser, stringify)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Example.Hooks.UseInitializer (UseInitializer, useInitializer)
import Halogen.Hooks (Hook, StateToken, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

type UseLocalStorage' a hooks = UseEffect (UseInitializer (UseState (Either String a) hooks))

foreign import data UseLocalStorage :: Type -> Type -> Type

type StorageInterface a =
  { key :: Key
  , defaultValue :: a
  , toJson :: a -> Json
  , fromJson :: Json -> Either String a
  }

-- | A key for a cell in local storage
newtype Key = Key String

derive newtype instance eqKey :: Eq Key

useLocalStorage
  :: forall slots output m a
   . MonadEffect m
  => Eq a
  => StorageInterface a
  -> Hook slots output m (UseLocalStorage a) (Either String a /\ StateToken (Either String a))
useLocalStorage { key, defaultValue, toJson, fromJson } = Hooks.publish hook
  where
  hook :: Hook slots output m (UseLocalStorage' a) (Either String a /\ StateToken (Either String a))
  hook = Hooks.do
    value /\ valueState <- Hooks.useState (Right defaultValue)

    let Key k = key

    useInitializer do
      storage <- liftEffect (localStorage =<< window)
      mbItem <- liftEffect (getItem k =<< localStorage =<< window)
      mbItem # maybe
        (liftEffect $ setItem k (stringify (toJson defaultValue)) storage)
        (\item -> Hooks.put valueState $ jsonParser item >>= fromJson)

    Hooks.captures { key, value } Hooks.useTickEffect do
      value' <- Hooks.get valueState
      storage <- liftEffect (localStorage =<< window)
      for_ value' \v -> liftEffect $ setItem k (stringify (toJson v)) storage
      pure Nothing

    Hooks.pure (Tuple value valueState)
