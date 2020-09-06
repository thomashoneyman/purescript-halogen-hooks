module Halogen.Hooks.Hook where

import Prelude hiding (bind, discard, pure)

import Control.Applicative as Applicative
import Control.Monad.Freed (Free)
import Halogen.Hooks.Internal.UseHookF (UseHookF)
import Unsafe.Coerce (unsafeCoerce)

-- | A function which has access to primitive and custom hooks like UseState,
-- | UseEffect, UseRef, and UseMemo. Hook functions can be used to implement
-- | reusable, stateful logic and to implement Halogen components.
-- |
-- | Functions of this type should be constructed using the Hooks API exposed
-- | by `Halogen.Hooks`.
newtype Hook m (h :: HookType) a = Hook (Free (UseHookF m) a)

derive newtype instance functorHook :: Functor (Hook m h)

-- | The kind of types used in Hooks; primitive Hooks already have this kind,
-- | and Hooks of your own should be foreign imported data types that are also
-- | types of this kind:
-- |
-- | ```purs
-- | foreign import data UseX :: Hooks.HookType
-- | ```
foreign import kind HookType

-- | A proxy used to provide type information for types of kind `HookType` when
-- | there is not a value available for the type. Used in the same situations
-- | you might reach for the `Type.Proxy` module, but usable for types of kind
-- | `HookType` instead of types of kind `Type`.
data HProxy (h :: HookType) = HProxy

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

-- | A class for asserting two `HookType`s are the same. Used so that you can
-- | provide a type synonym to `HookNewtype`:
-- |
-- | ```purs
-- | foreign import data UseX :: HookType
-- |
-- | type UseX' = UseState Int <> UseEffect <> Pure
-- |
-- | instance newtypeUseX :: HookEquals h UseX' => HookNewtype UseX h
-- | ```
-- |
-- | This is especially useful with large stacks of hooks or with hooks that
-- | themselves use type synonyms (like type synonyms for records).
class HookEquals (a :: HookType) (b :: HookType) | a -> b, b -> a

instance hookRefl :: HookEquals a a

-- | Make a stack of hooks opaque to improve error messages and ensure internal
-- | types like state are not leaked outside the module where the hook is defined.
-- |
-- | We recommend using this for any custom hooks you define.
-- |
-- | ```purs
-- | foreign import data MyHook :: HookType
-- |
-- | instance newtypeMyHook :: HookNewtype MyHook (UseState Int <> Pure)
-- |
-- | useMyHook :: forall m. Hook m MyHook Int
-- | useMyHook = Hooks.wrap Hooks.do
-- |   ... -- hook definition goes here
-- | ```
wrap :: forall h h' m a. HookNewtype h' h => Hook m h a -> Hook m h' a
wrap = unsafeCoerce -- only necessary because we can't use `Newtype`

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
