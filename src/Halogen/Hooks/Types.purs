module Halogen.Hooks.Types where

type ComponentTokens q ps o =
  { queryToken :: QueryToken q
  , slotToken :: SlotToken ps
  , outputToken :: OutputToken o
  }

foreign import data QueryToken :: (Type -> Type) -> Type

foreign import data SlotToken :: # Type -> Type

foreign import data OutputToken :: Type -> Type

foreign import data MemoValues :: Type

newtype StateToken state = StateToken Int
