module Halogen.Hooks.Hook
  ( Hook
  , HookAppend
  , type (<>)
  , Pure
  , class HookNewtype
  , bind
  , discard
  , pure
  , unsafeFromHook
  , unsafeToHook
  ) where

import Prelude hiding (bind, discard, pure)

import Control.Applicative as Applicative
import Control.Monad.Free (Free, liftF)
import Halogen.Hooks.Internal.UseHookF (UseHookF)
import Halogen.Hooks.Types (HookType)

-- | A function which has access to primitive and custom hooks like UseState,
-- | UseEffect, UseRef, and UseMemo. Hook functions can be used to implement
-- | reusable, stateful logic and to implement Halogen components.
-- |
-- | Functions of this type should be constructed using the Hooks API exposed
-- | by `Halogen.Hooks`.
newtype Hook :: (Type -> Type) -> HookType -> Type -> Type
newtype Hook m h a = Hook (Free (UseHookF m) a)

type role Hook representational nominal representational

derive instance Functor (Hook m h)

unsafeToHook :: forall m h a. UseHookF m a -> Hook m h a
unsafeToHook = Hook <<< liftF

unsafeFromHook :: forall m h a. Hook m h a -> Free (UseHookF m) a
unsafeFromHook (Hook hookF) = hookF

-- | A type for listing several Hook types in order. Typically this is used via
-- | the operator `<>`.
-- |
-- | ```purs`
-- | import Halogen.Hooks (type (<>))
-- |
-- | type UseStateEffect = UseState Int <> UseEffect <> Pure
-- |
-- | -- using  to the
-- | type UseStateEffect = HookAppend (UseState Int) (HookAppend UseEffect Nil)
-- | ```
foreign import data HookAppend :: HookType -> HookType -> HookType

-- | `HookAppend` as an infix operator
infixr 1 type HookAppend as <>

-- | The `HookType` used for `pure`, which lifts an arbitrary value into `Hook`.
-- |
-- | ```purs`
-- | type UseX = UseState Int <> UseEffect <> Pure
-- | ```
foreign import data Pure :: HookType

-- | A class for asserting that one `HookType` can be "unwrapped" to produce
-- | the other. This class is used to turn a list of Hooks into a new opaque
-- | Hook in conjunction with `wrap`:
-- |
-- | ```purs
-- | foreign import data UseX :: HookType
-- |
-- | instance newtypeUseX :: HookNewtype UseX (UseState Int <> UseEffect <> Pure)
-- |
-- | useX :: forall m. Hook m UseX Int
-- | useX = Hooks.wrap Hooks.do
-- |   -- ... use useState, useEffect in the implementation
-- | ```
class HookNewtype (a :: HookType) (b :: HookType) | a -> b

-- | For use with qualified-do.
-- |
-- | ```purs
-- | import Halogen.Hooks as Hooks
-- |
-- | useMyHook = Hooks.do
-- |   -- bind is necessary to use do-syntax with Hooks
-- |   ... <- Hooks.useState ...
-- | ```
bind :: forall h h' m a b. Hook m h a -> (a -> Hook m h' b) -> Hook m (h <> h') b
bind (Hook ma) f = Hook $ ma >>= \a -> case f a of Hook mb -> mb

-- | For use with qualified-do.
-- |
-- | ```purs
-- | import Halogen.Hooks as Hooks
-- |
-- | useMyHook = Hooks.do
-- |   ...
-- |   -- discard is necessary to use do-syntax with Hooks
-- |   Hooks.useLifecycleEffect ...
-- | ```
discard :: forall h h' m a. Hook m h Unit -> (Unit -> Hook m h' a) -> Hook m (h <> h') a
discard = bind

-- | For use with qualified-do:
-- |
-- | ```purs
-- | import Halogen.Hooks as Hooks
-- |
-- | useMyHook = Hooks.do
-- |   ...
-- |   Hooks.pure ...
-- | ```
pure :: forall h m a. a -> Hook m h a
pure = Hook <<< Applicative.pure
