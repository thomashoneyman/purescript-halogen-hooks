-- | This module implements the entire Halogen Hooks library. It's implemented as
-- | a monolithic module so that only types and functions meant for users are
-- | exported.
module Halogen.Hooks
  ( -- Hook API
    useState
  , UseState
  , useLifecycleEffect
  , useTickEffect
  , UseEffect
  , useQuery
  , UseQuery
  , useMemo
  , UseMemo
  , useRef
  , UseRef

  -- Hook types and helpers
  , module Halogen.Hooks.Component
  , module Halogen.Hooks.Hook
  , module Halogen.Hooks.HookM
  , module Halogen.Hooks.Types

  -- Helpers
  , captures
  , capturesWith
  , wrap

  -- Qualified do
  , bind
  , discard
  , pure
  )
where

import Halogen.Hooks.HookM

import Control.Applicative as Applicative
import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Free (liftF)
import Data.Eq (class Eq)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Ref (Ref)
import Halogen.Hooks.Component (component, componentWithTokens)
import Halogen.Hooks.Hook (Hook, Hooked(..))
import Halogen.Hooks.Internal.Types as IT
import Halogen.Hooks.Internal.UseHookF (UseHookF(..))
import Halogen.Hooks.Types (MemoValues, QueryToken, StateToken(..))
import Prelude (Unit, unit, ($), (<<<), (==))
import Unsafe.Coerce (unsafeCoerce)

foreign import data UseState :: Type -> Type -> Type

-- | A Hook providing an independent state and a token usable to modify that state.
-- |
-- | ```purs
-- | Hooks.do
-- |   -- Create a new state with `useState`
-- |   stateA /\ stateToken <- Hooks.useState initialState
-- |   intState /\ intStateToken <- Hooks.useState 0
-- |
-- |   -- Perform state updates in `HookM` using the state token
-- |   let
-- |     update newState = do
-- |       Hooks.put stateTokenA stateA
-- |       Hooks.modify_ stateTokenB (_ + 10)
-- | ```
useState :: forall state m. state -> Hook m (UseState state) (state /\ StateToken state)
useState initialState = Hooked $ Indexed $ liftF $ UseState initialState' interface
  where
  initialState' :: IT.StateValue
  initialState' = IT.toStateValue initialState

  interface :: IT.StateValue /\ StateToken IT.StateValue -> state /\ StateToken state
  interface (value /\ token) = IT.fromStateValue value /\ unsafeCoerce token

foreign import data UseEffect :: Type -> Type

-- | A Hook providing the ability to run an effect the first time the hook is run,
-- | which can return another effect to run the last time the hook is run. This
-- | is equivalent to component initializers and finalizers.
-- |
-- | If you would like to run your effect after every render, see `useTickEffect`.
useLifecycleEffect :: forall m. HookM m (Maybe (HookM m Unit)) -> Hook m UseEffect Unit
useLifecycleEffect fn = Hooked $ Indexed $ liftF $ UseEffect Nothing fn unit

-- | A Hook providing the ability to run an effect after every render, which
-- | includes the first time the hook is run.
-- |
-- | This Hook can be given an array of memo values as a performance optimization.
-- | If the provided array is empty, the effect will run on every render. If the
-- | array contains values, then the effect will only run on renders in which one
-- | or more of the memo values have changed.
-- |
-- | To run an effect on every render:
-- |
-- | ```purs
-- | Hooks.captures {} Hooks.useTickEffect do
-- |   ...
-- | ```
-- |
-- | To run an effect on the first render and when a particular value has changed:
-- |
-- | ```purs
-- | Hooks.captures { memoA, memoB } Hooks.useTickEffect do
-- |   ...
-- | ```
useTickEffect
  :: forall m
   . MemoValues
  -> HookM m (Maybe (HookM m Unit))
  -> Hook m UseEffect Unit
useTickEffect memos fn = Hooked $ Indexed $ liftF $ UseEffect (Just memos) fn unit

foreign import data UseQuery :: Type -> Type

-- | A Hook providing the ability to receive and evaluate queries from a parent
-- | component. Only usable in components constructed with `componentWithQuery`,
-- | not in arbitrary hooks; the request/response nature of queries means they
-- | only make sense in the context of a component.
-- |
-- | If this Hook is used multiple times in a single component definition, only
-- | the last use will take effect.
useQuery
  :: forall query m
   . QueryToken query
  -> (forall a. query a -> HookM m (Maybe a))
  -> Hook m UseQuery Unit
useQuery token handler = Hooked $ Indexed $ liftF $ UseQuery token' handler' unit
  where
  token' :: QueryToken IT.QueryValue
  token' = unsafeCoerce token

  handler' :: forall a. IT.QueryValue a -> HookM m (Maybe a)
  handler' = handler <<< IT.fromQueryValue

foreign import data UseMemo :: Type -> Type -> Type

-- | A Hook providing the ability to memoize a particular value.
-- |
-- | When values are used in let bindings within the body of a Hook they are
-- | recomputed each time the Hook's body is evaluated (on every render). For
-- | values which are expensive to compute, you can either cache them in state
-- | (as you would with an ordinary Halogen component) or you can use `useMemo`.
-- |
-- | All dependencies used to compute the memoized value should be provided to
-- | the `captures` or `capturesWith` function. Consider defining your `useMemo`
-- | Hook in a `where` clause to ensure you don't omit something by accident,
-- | which will lead to stale values.
-- |
-- | ```purs
-- | -- before, computed on every render:
-- | Hooks.do
-- |   x /\ _ <- Hooks.useState 0
-- |   y /\ _ <- Hooks.useState ""
-- |   let expensiveValue = expensiveFunction x y
-- |
-- | -- after, computed only if `x` or `y` have changed:
-- | Hooks.do
-- |   x /\ _ <- useState 0
-- |   y /\ _ <- useState ""
-- |   expensiveValue <- useExpensive x y
-- |   ...
-- |   where
-- |   useExpensive deps@{ x, y } = Hooks.captures deps $ flip Hooks.useMemo \_ ->
-- |     expensiveFunction x y
-- | ```
useMemo :: forall m a. MemoValues -> (Unit -> a) -> Hook m (UseMemo a) a
useMemo memos fn = Hooked $ Indexed $ liftF $ UseMemo memos to from
  where
  to :: Unit -> IT.MemoValue
  to = IT.toMemoValue <<< fn

  from :: IT.MemoValue -> a
  from = IT.fromMemoValue

foreign import data UseRef :: Type -> Type -> Type

-- | A Hook providing the ability to use a mutable reference.
-- |
-- | This Hook returns the value of the mutable reference at the time the Hook
-- | was run, and the reference itself which can be read at any time. The value
-- | of the reference can be used for rendering, but any effectful computations
-- | in `HookM` should read the value of the reference to guarantee an up-to-date
-- | value.
-- |
-- | ```purs
-- | value /\ ref <- Hooks.useRef initialValue
-- |
-- | -- Read and write the ref in effectful code
-- | Hooks.captures {} Hooks.useTickEffect do
-- |   current <- liftEffect $ Ref.read ref
-- |   -- ... use the current value
-- |
-- | -- Use the last-read value directly in render code
-- | Hooks.pure $ HH.text (show value)
-- | ```
useRef :: forall m a. a -> Hook m (UseRef a) (a /\ Ref a)
useRef initialValue = Hooked $ Indexed $ liftF $ UseRef initialValue' interface
  where
  initialValue' :: IT.RefValue
  initialValue' = IT.toRefValue initialValue

  interface :: IT.RefValue /\ Ref IT.RefValue -> a /\ Ref a
  interface (value /\ ref) = IT.fromRefValue value /\ (unsafeCoerce :: Ref IT.RefValue -> Ref a) ref

-- | Hide a stack of hooks behind a newtype to improve error messages and ensure
-- | internal types like state are not leaked outside the module where the Hook
-- | is defined.
-- |
-- | We recommend using this for any custom hooks you define.
-- |
-- | ```purs
-- | newtype MyHook hooks = MyHook (UseState Int hooks)
-- |
-- | derive instance newtypeMyHook :: Newtype (MyHook hooks) _
-- |
-- | useMyHook :: forall ps o m. Hook ps o m MyHook Int
-- | useMyHook = Hooks.wrap Hooks.do
-- |   ... -- hook definition goes here
-- | ```
wrap
  :: forall hooks internalHooks wrappedHooks m a
   . Newtype wrappedHooks internalHooks
  => Hooked m hooks internalHooks a
  -> Hooked m hooks wrappedHooks a
wrap hook = ibind hook (Hooked <<< Indexed <<< Applicative.pure)

-- | Used to improve performance for hooks which may be expensive to run on
-- | many renders (like `useTickEffect` and `useMemo`). Uses a value equality
-- | check to verify values have changed before re-running a function.
-- |
-- | Some values may be expensive to check for value equality. You can optimize
-- | this by only checking a sub-part of your captured values using `capturesWith`
captures :: forall memos a. Eq (Record memos) => Record memos -> (MemoValues -> a) -> a
captures memos fn = fn $ IT.toMemoValues $ IT.toMemoValuesImpl { eq: (==), memos }

-- | Like `captures`, but without an `Eq` constraint. Use when you only want to
-- | check part of a captured value for equality or when your captured values
-- | don't have Eq instances.
-- |
-- | This function can recreate the usual `captures`:
-- |
-- | ```purs
-- | Hooks.captures { memoA, memoB } == Hooks.capturesWith eq { memoA, memoB }
-- | ```
-- |
-- | You can also choose to improve performance by testing only a sub-part
-- | of your memoized values. Remember that this equality check is used to
-- | decide whether to re-run your effect or function, so make sure to test
-- | everything in your captures list.
-- |
-- | ```purs
-- | let
-- |   customEq memoA memoB =
-- |     memoA.user.id == memoB.user.id && memoA.data == memoB.data
-- |
-- | Hooks.capturesWith customEq { user, data }
-- | ```
capturesWith
  :: forall memos a
   . (Record memos -> Record memos -> Boolean)
  -> Record memos
  -> (MemoValues -> a)
  -> a
capturesWith memosEq memos fn =
  fn $ IT.toMemoValues $ IT.toMemoValuesImpl { eq: memosEq, memos }

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure
