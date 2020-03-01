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
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Example.Hooks.UseInitializer (UseInitializer, useInitializer)
import Halogen.EvalHookM (StateToken)
import Halogen.EvalHookM as EH
import Halogen.Hook (Hook, UseEffect, UseState)
import Halogen.Hook as Hook
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
  :: forall ps o m a
   . MonadEffect m
  => Eq a
  => StorageInterface a
  -> Hook ps o m (UseLocalStorage a) (Either String a /\ StateToken (Either String a))
useLocalStorage { key, defaultValue, toJson, fromJson } = Hook.coerce hook
  where
  hook :: Hook ps o m (UseLocalStorage' a) (Either String a /\ StateToken (Either String a))
  hook = Hook.do
    value /\ valueState <- Hook.useState (Right defaultValue)

    let Key k = key

    useInitializer do
      storage <- liftEffect (localStorage =<< window)
      mbItem <- liftEffect (getItem k =<< localStorage =<< window)
      case mbItem of
        Nothing ->
          liftEffect $ setItem k (stringify (toJson defaultValue)) storage
        Just item ->
          EH.put valueState (jsonParser item >>= fromJson)

    Hook.captures { key, value } Hook.useTickEffect do
      storage <- liftEffect (localStorage =<< window)
      for_ (map toJson value) \v ->
        liftEffect $ setItem k (stringify v) storage
      pure Nothing

    Hook.pure (Tuple value valueState)
