module Halogen.Hooks.Internal.UseHookF where

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Free (Free)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Effect.Ref (Ref)
import Halogen.Hooks.HookM (HookM, StateToken)
import Halogen.Hooks.Types (MemoValues, QueryToken)
import Halogen.Hooks.Internal.Types (MemoValue, QueryValue, RefValue, StateValue)
import Prelude (class Functor, Unit)

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
