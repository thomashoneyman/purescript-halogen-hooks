module Halogen.Hooks.Hook where

import Prelude hiding (bind, discard, pure)

import Control.Applicative as Applicative
import Control.Monad.Free (Free)
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

-- | A type for listing several `HookType`s in order. Typically used via the
-- | type-level operator (<>):
-- |
-- | ```purs`
-- | import Halogen.Hooks (type (<>))
-- |
-- | type UseStateEffect = UseState Int <> UseEffect <> Nil
-- |
-- | -- equivalent to
-- | type UseStateEffect = Hooked (UseState Int) (Hooked UseEffect Nil)
-- | ```
foreign import data Hooked :: HookType -> HookType -> HookType

infixr 1 type Hooked as <>

-- | A `HookType` that can be used to end a list of `HookType`s produced using
-- | `Hooked`:
-- |
-- | ```purs`
-- | type UseX = UseState Int <> UseEffect <> Nil
-- | ```
foreign import data Nil :: HookType

-- | A class for asserting that one `HookType` can be "unwrapped" to produce
-- | the other. This class is used to turn a list of Hooks into a new opaque
-- | Hook in conjunction with `wrap`:
-- |
-- | ```purs
-- | foreign import data UseX :: HookType
-- |
-- | instance newtypeUseX :: HookNewtype UseX (UseState Int <> UseEffect <> Nil)
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
-- | type UseX' = UseState Int <> UseEffect <> Nil
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
-- | instance newtypeMyHook :: HookNewtype MyHook (UseState Int <> Nil)
-- |
-- | useMyHook :: forall m. Hook m MyHook Int
-- | useMyHook = Hooks.wrap Hooks.do
-- |   ... -- hook definition goes here
-- | ```
wrap :: forall h h' m a. HookNewtype h' h => Hook m h a -> Hook m h' a
wrap = unsafeCoerce -- Safe implementation exists with bind / pure, but we use `HookType`

-- | For use with qualified-do.
bind :: forall h h' m a b. Hook m h a -> (a -> Hook m h' b) -> Hook m (h <> h') b
bind (Hook ma) f = Hook $ ma >>= \a -> case f a of Hook mb -> mb

-- | For use with qualified-do.
discard :: forall h h' m a. Hook m h Unit -> (Unit -> Hook m h' a) -> Hook m (h <> h') a
discard = bind

-- | For use with qualified-do.
pure :: forall h m a. a -> Hook m h a
pure = Hook <<< Applicative.pure
