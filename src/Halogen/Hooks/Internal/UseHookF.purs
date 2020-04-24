module Halogen.Hooks.Internal.UseHookF where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Effect.Ref (Ref)
import Halogen.Hooks.HookM (HookM, StateToken)
import Halogen.Hooks.Types (MemoValues, QueryToken)
import Halogen.Hooks.Internal.Types (MemoValue, QueryValue, RefValue, StateValue)

-- | The Hook API: a set of primitive building blocks for writing stateful logic
-- | in Halogen. These should not be used directly; the hook functions supplied
-- | in `Hooks` should be used instead.
data UseHookF ps o m a
  = UseState StateValue ((StateValue /\ StateToken StateValue) -> a)
  | UseEffect (Maybe MemoValues) (HookM ps o m (Maybe (HookM ps o m Unit))) a
  | UseQuery (QueryToken QueryValue) (forall b. QueryValue b -> HookM ps o m (Maybe b)) a
  | UseMemo MemoValues (Unit -> MemoValue) (MemoValue -> a)
  | UseRef RefValue ((RefValue /\ Ref RefValue) -> a)

derive instance functorUseHookF :: Functor (UseHookF ps o m)
