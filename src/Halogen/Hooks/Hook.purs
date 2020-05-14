module Halogen.Hooks.Hook where

import Prelude

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Free (Free)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Halogen.Hooks.Internal.UseHookF (UseHookF)

-- | A function which has access to primitive and custom hooks like UseState,
-- | UseEffect, UseRef, and UseMemo. Hook functions can be used to implement
-- | reusable, stateful logic and to implement Halogen components.
-- |
-- | Functions of this type should be constructed using the Hooks API exposed
-- | by `Halogen.Hooks`.
type Hook m (newHook :: Type -> Type) a
  = forall hooks. Hooked m hooks (newHook hooks) a

-- | A largely internal type which underlies the `Hook` type. Used when the first
-- | type variable of the indexed monad, `hooks`, cannot be hidden.
newtype Hooked m pre post a = Hooked (Indexed (Free (UseHookF m)) pre post a)

instance functorHooked :: Functor (Hooked m pre post) where
  map f (Hooked (Indexed m)) = Hooked (Indexed (map f m))

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked m)
derive newtype instance ixBindIndexed :: IxBind (Hooked m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked m)
