# Writing Your Own Hooks

This chapter is partially complete. You can learn how to write your own Hook types here, but for more details on writing Hooks please [see the section on implementing your own Hooks](https://thomashoneyman.com/articles/introducing-halogen-hooks#implementing-usewindowwidth) in Introducing Halogen Hooks.

## How do I write a new Hook type?

Hooks are uniquely identified by a type of the kind `HookType`. All primitive Hooks in this library have this kind, but what about when you want to write your own? Let's walk through how to define your own hook types. We'll use a small example for demonstration.

Let's say we want to define a Hook which, given a value, returns the value from the _previous_ evaluation. This can be useful to manually check if a value has changed between renders, for example.

```purs
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (type (<>), Hook, UseEffect, UseRef)
import Halogen.Hooks as Hooks

type UsePrevious a = UseRef (Maybe a) <> UseEffect <> Hooks.Pure

usePrevious :: forall m a. MonadAff m => Eq a => a -> Hook m (UsePrevious a) (Maybe a)
usePrevious value = Hooks.do
  prev /\ ref <- Hooks.useRef Nothing

  Hooks.captures { } Hooks.useTickEffect do
    liftEffect $ Ref.write (Just value) ref
    pure Nothing

  Hooks.pure prev
```

This custom Hook compiles. The type synonym indicates that our Hook uses a mutable reference, then an effect, and then produces a value via `Hooks.pure`. We use the type `<>` to list Hooks in order.

Our type synonym is adequate to implement the Hook, but it isn't a best practice. Instead, almost all custom Hooks should implement their own custom `HookType`. `UseRef`, `UseState`, `Pure`, and the other primitives from Halogen Hooks are each unique `HookType`s. Here's why:

1. If there were a compile-time error involving this Hook then the error message would contain the full list of Hooks; in the real world, with Hooks that contain other Hooks, this can lead to unreadable error messages.
2. Hooks must always run in the same order in each evaluation, which is checked by ensuring the evaluation uses the same hook types in order. If you have two Hooks that have an identical list of Hook types, but which do different things, then you can subvert this check accidentally. This is admittedly quite unlikely, but it's a reason to rely on custom Hook types instead of the structure of the Hook's internals.

Let's adjust this example so that it uses a custom Hook type:

```purs
import Halogen.Hooks (class HookNewtype, type (<>), Hook, UseEffect, UseRef)

foreign import data UsePrevious :: Type -> Hooks.HookType

instance newtypeUsePrevious
  :: HookNewtype (UsePrevious a) (UseRef (Maybe a) <> UseEffect <> Hooks.Pure)

usePrevious :: forall m a. MonadAff m => Eq a => a -> Hook m (UsePrevious a) (Maybe a)
usePrevious value = Hooks.wrap hook
  where
  hook :: Hook m (UseRef (Maybe a) <> UseEffect <> Hooks.Pure) (Maybe a)
  hook = Hooks.do ...
```

We can implement a custom Hook type in three steps:

1. Use a foreign import to get a new data type of the kind `HookType`, which should follow the naming convention `Use<X>`. Here, we named it `UsePrevious` and gave it the kind `Type -> HookType`, indicating that this Hook type takes an argument.
2. Use the `HookNewtype` type to indicate what Hooks this `HookType` uses internally. Here, we indicated that `UsePrevious a` is a custom Hook type for `UseRef (Maybe a) <> UseEffect <> Pure`.
3. Use the `Hooks.wrap` function to turn a Hook using the right internal Hooks into a Hook which uses your new custom `HookType`. Here, we've written our Hook implementation in a `where` block with a type signature that shows the Hooks we're using internally. I recommend writing Hook implementations in `where` blocks with type signatures to aid the compiler with type inference.

Once again, we have a Hook that compiles. This time, though, our worries about unreadable errors and revealing Hook internals are gone. Still, we can improve things: we've written out our Hook internals twice: once in our `HookNewtype` and once again in the implementation.

Let's clean things up by writing our internal Hook types in a type synonym. In PureScript 0.14:

```purs
foreign import data UsePrevious :: Type -> Hooks.HookType

type UsePrevious' a = UseRef (Maybe a) <> UseEffect <> Hooks.Pure

instance newtypeUsePrevious :: HookNewtype (UsePrevious a) (UsePrevious' a)

usePrevious :: forall m a. MonadAff m => Eq a => a -> Hook m (UsePrevious a) (Maybe a)
usePrevious value = Hooks.wrap hook
  where
  hook :: Hook m (UsePrevious' a) (Maybe a)
  hook = Hooks.do ...
```

This is much better! In PureScript 0.14 this is the best practice for how to write a Hook like this one. However, in PureScript 0.13.x you cannot write the `HookNewtype` instance for a type synonym. Instead, you'll first have to use the `HookEquals` class to say that the type synonym is equivalent to some type `h`, and then write the `HookNewtype` instance for that type.

Here's the same Hook implemented in PureScript v0.13.x:

```purs
import Halogen.Hooks (class HookEquals, class HookNewtype, type (<>), Hook, UseEffect, UseRef)

foreign import data UsePrevious :: Type -> Hooks.HookType

type UsePrevious' a = UseRef (Maybe a) <> UseEffect <> Hooks.Pure

instance newtypeUsePrevious
  :: HookEquals (UsePrevious' a) h => HookNewtype (UsePrevious a) h

usePrevious :: forall m a. MonadAff m => Eq a => a -> Hook m (UsePrevious a) (Maybe a)
usePrevious value = Hooks.wrap hook
  where
  hook :: Hook m (UsePrevious' a) (Maybe a)
  hook = Hooks.do ...
```
