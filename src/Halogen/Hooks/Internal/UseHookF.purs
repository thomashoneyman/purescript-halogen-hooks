module Halogen.Hooks.Internal.UseHookF where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Effect.Ref (Ref)
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.Internal.Types (MemoValue, QueryValue, RefValue, StateValue)
import Halogen.Hooks.Types (MemoValues, QueryToken, StateId)

-- | The Hook API: a set of primitive building blocks for writing stateful logic
-- | in Halogen. These should not be used directly; the hook functions supplied
-- | in `Hooks` should be used instead.
data UseHookF m a
  = UseState StateValue ((StateValue /\ StateId StateValue) -> a)
  | UseEffect (Maybe MemoValues) (HookM m (Maybe (HookM m Unit))) a
  | UseQuery (QueryToken QueryValue) (forall b. QueryValue b -> HookM m (Maybe b)) a
  | UseMemo MemoValues (Unit -> MemoValue) (MemoValue -> a)
  | UseRef RefValue ((RefValue /\ Ref RefValue) -> a)

derive instance Functor (UseHookF m)
