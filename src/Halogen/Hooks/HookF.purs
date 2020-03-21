module Halogen.Hooks.HookF
  ( Hook
  , Hooked(..)
  , HookF(..)
  ) where

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Free (Free)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed)
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import Halogen.Hooks.HalogenHookM (HalogenHookM, StateToken)
import Halogen.Hooks.Internal.Types (MemoValue, MemoValues, QueryToken, QueryValue, RefValue, StateValue)
import Prelude (class Functor, Unit)

-- | The Hook API: a set of primitive building blocks that can be used on their
-- | own to share stateful logic or used to create new hooks.
data HookF ps o m a
  = UseState StateValue ({ token :: StateToken StateValue, value :: StateValue } -> a)
  | UseEffect (Maybe MemoValues) (HalogenHookM ps o m (Maybe (HalogenHookM ps o m Unit))) a
  | UseQuery (QueryToken QueryValue) (forall b. QueryValue b -> HalogenHookM ps o m (Maybe b)) a
  | UseMemo MemoValues (Unit -> MemoValue) (MemoValue -> a)
  | UseRef RefValue ({ ref :: Ref RefValue, value :: RefValue } -> a)

derive instance functorHookF :: Functor (HookF ps o m)

newtype Hooked ps o m pre post a = Hooked (Indexed (Free (HookF ps o m)) pre post a)

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked ps o m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked ps o m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked ps o m)
derive newtype instance ixBindIndexed :: IxBind (Hooked ps o m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked ps o m)

type Hook ps o m (newHook :: Type -> Type) a
  = forall hooks. Hooked ps o m hooks (newHook hooks) a
