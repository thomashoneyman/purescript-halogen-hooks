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
import Halogen.Hooks (class HookEquals, class HookNewtype, type (<>), Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

foreign import data UseLocalStorage :: Type -> Hooks.HookType

type UseLocalStorage' a =
  UseState (Either String a)
    <> UseInitializer
    <> UseEffect
    <> Hooks.Nil

instance newtypeUseLocalStorage
  :: HookEquals x (UseLocalStorage' a)
  => HookNewtype (UseLocalStorage a) x

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
  :: forall m a
   . MonadEffect m
  => Eq a
  => StorageInterface a
  -> Hook m (UseLocalStorage a) (Either String a /\ ((Either String a -> Either String a) -> HookM m Unit))
useLocalStorage { key, defaultValue, toJson, fromJson } = Hooks.wrap Hooks.do
  state /\ stateId <- Hooks.useState (Right defaultValue)

  let Key k = key

  useInitializer do
    storage <- liftEffect (localStorage =<< window)
    mbItem <- liftEffect (getItem k storage)
    mbItem # maybe
      (liftEffect $ setItem k (stringify (toJson defaultValue)) storage)
      (\item -> Hooks.modify_ stateId \_ -> jsonParser item >>= fromJson)

  useWriteStorage { value: state, key: k }

  Hooks.pure (Tuple state (Hooks.modify_ stateId))
  where
  useWriteStorage deps = Hooks.captures deps Hooks.useTickEffect do
    liftEffect do
      storage <- localStorage =<< window
      for_ deps.value \v -> setItem deps.key (stringify (toJson v)) storage
    pure Nothing
