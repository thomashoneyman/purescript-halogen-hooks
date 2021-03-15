module Halogen.Hooks.Internal.Types where

import Halogen.Hooks.Types
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)

foreign import data StateValue :: Type

toStateValue :: forall state. state -> StateValue
toStateValue = unsafeCoerce

fromStateValue :: forall state. StateValue -> state
fromStateValue = unsafeCoerce

foreign import data QueryValue :: Type -> Type

toQueryValue :: forall q a. q a -> QueryValue a
toQueryValue = unsafeCoerce

fromQueryValue :: forall q a. QueryValue a -> q a
fromQueryValue = unsafeCoerce

foreign import data SlotType :: Row Type

foreign import data OutputValue :: Type

toOutputValue :: forall output. output -> OutputValue
toOutputValue = unsafeCoerce

fromOutputValue :: forall output. OutputValue -> output
fromOutputValue = unsafeCoerce

foreign import data MemoValue :: Type

type MemoValuesImpl =
  { eq :: Object MemoValue -> Object MemoValue -> Boolean
  , memos :: Object MemoValue
  }

foreign import _memoValuesImpl :: forall r. (Record r -> Record r -> Boolean) -> Record r -> MemoValuesImpl

toMemoValuesImpl
  :: forall memos
   . { eq :: Record memos -> Record memos -> Boolean, memos :: Record memos }
  -> MemoValuesImpl
toMemoValuesImpl { eq, memos } = _memoValuesImpl eq memos

toMemoValues :: MemoValuesImpl -> MemoValues
toMemoValues = unsafeCoerce

fromMemoValues :: MemoValues -> MemoValuesImpl
fromMemoValues = unsafeCoerce

toMemoValue :: forall memo. memo -> MemoValue
toMemoValue = unsafeCoerce

fromMemoValue :: forall memo. MemoValue -> memo
fromMemoValue = unsafeCoerce

foreign import data RefValue :: Type

toRefValue :: forall a. a -> RefValue
toRefValue = unsafeCoerce

fromRefValue :: forall a. RefValue -> a
fromRefValue = unsafeCoerce
