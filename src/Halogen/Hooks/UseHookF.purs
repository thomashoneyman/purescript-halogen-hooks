module Halogen.Hooks.UseHookF
  ( Hook
  , Hooked(..)
  , UseHookF(..)
  ) where

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
import Halogen.Hooks.Internal.Types (MemoValue, MemoValues, QueryToken, QueryValue, RefValue, StateValue)
import Prelude (class Functor, Unit)

-- | The Hook API: a set of primitive building blocks for writing stateful logic
-- | in Halogen.
data UseHookF ps o m a
  = UseState StateValue ((StateValue /\ StateToken StateValue) -> a)
  | UseEffect (Maybe MemoValues) (HookM ps o m (Maybe (HookM ps o m Unit))) a
  | UseQuery (QueryToken QueryValue) (forall b. QueryValue b -> HookM ps o m (Maybe b)) a
  | UseMemo MemoValues (Unit -> MemoValue) (MemoValue -> a)
  | UseRef RefValue ((RefValue /\ Ref RefValue) -> a)

derive instance functorUseHookF :: Functor (UseHookF ps o m)

-- | A largely internal type which underlies the `Hook` type. Used when the first
-- | type variable of the indexed monad, `hooks`, cannot be hidden.
newtype Hooked ps o m pre post a = Hooked (Indexed (Free (UseHookF ps o m)) pre post a)

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked ps o m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked ps o m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked ps o m)
derive newtype instance ixBindIndexed :: IxBind (Hooked ps o m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked ps o m)

-- | A function which has access to primitive and custom hooks like UseState,
-- | UseEffect, UseRef, and UseMemo. Hook functions can be used to implement
-- | reusable, stateful logic and to implement Halogen components.
-- |
-- | Functions of this type should be constructed using the Hooks API exposed
-- | by `Halogen.Hooks`.
type Hook ps o m (newHook :: Type -> Type) a
  = forall hooks. Hooked ps o m hooks (newHook hooks) a
