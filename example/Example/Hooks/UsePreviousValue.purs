module Example.Hooks.UsePreviousValue
  ( usePreviousValue
  , UsePreviousValue
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen.EvalHookM as EH
import Halogen.Hook (Hook, UseEffect, UseState)
import Halogen.Hook as Hook

foreign import data UsePreviousValue :: Type -> Type -> Type

type UsePreviousValue' a hooks = UseEffect (UseState a hooks)

-- TODO: Introduce a useRef hook which allows this to happen without state updates
usePreviousValue :: forall ps o m a. MonadAff m => Eq a => a -> Hook ps o m (UsePreviousValue a) a
usePreviousValue value = Hook.coerce hook
  where
  hook :: Hook ps o m (UsePreviousValue' a) a
  hook = Hook.do
    prevValue /\ prevValueState <- Hook.useState value

    Hook.captures { value } Hook.useTickEffect do
      EH.put prevValueState value
      pure Nothing

    Hook.pure prevValue
