module Halogen.Hook
  ( useState
  , UseState
  , useLifecycleEffect
  , useTickEffect
  , UseEffect
  , useQuery
  , UseQuery
  , captures
  , capturesWith
  , Hook
  , Hooked
  , coerce
  , component
  , componentWithQuery
  , bind
  , discard
  , pure
  )
where

import Halogen.EvalHookM

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Indexed (class IxMonad)
import Data.Array as Array
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (for_)
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Eq, class Functor, type (~>), Unit, map, mempty, not, unit, void, when, ($), (+), (-), (<), (<<<), (==), (||))
import Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | The Hook API: a set of primitive building blocks that can be used on their
-- | own to share stateful logic or used to create new hooks.
data HookF ps o m a
  = UseState StateValue (StateInterface -> a)
  | UseEffect (Maybe MemoValues) (EvalHookM ps o m (Maybe (EvalHookM ps o m Unit))) a
  | UseQuery (QueryToken QueryValue) (forall b. QueryValue b -> EvalHookM ps o m (Maybe b)) a

derive instance functorHookF :: Functor (HookF ps o m)

newtype Hooked ps o m pre post a = Hooked (Indexed (Free (HookF ps o m)) pre post a)

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked ps o m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked ps o m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked ps o m)
derive newtype instance ixBindIndexed :: IxBind (Hooked ps o m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked ps o m)

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

type Hook ps o m (newHook :: Type -> Type) a
  = forall hooks. Hooked ps o m hooks (newHook hooks) a

--  State

foreign import data UseState :: Type -> Type -> Type

type StateInterface =
  { stateToken :: StateToken StateValue
  , getState :: StateValue
  }

useState :: forall state ps o m. state -> Hook ps o m (UseState state) (state /\ StateToken state)
useState initialState = Hooked $ Indexed $ liftF $ UseState initialState' interface
  where
  initialState' :: StateValue
  initialState' = toStateValue initialState

  interface :: StateInterface -> state /\ StateToken state
  interface { getState, stateToken } = fromStateValue getState /\ unsafeCoerce stateToken

-- Query

foreign import data UseQuery :: Type -> Type

useQuery
  :: forall q ps o m
   . QueryToken q
  -> (forall a. q a -> EvalHookM ps o m (Maybe a))
  -> Hook ps o m UseQuery Unit
useQuery token handler = Hooked $ Indexed $ liftF $ UseQuery token' handler' unit
  where
  token' :: QueryToken QueryValue
  token' = unsafeCoerce token

  handler' :: forall a. QueryValue a -> EvalHookM ps o m (Maybe a)
  handler' = handler <<< fromQueryValue

-- Lifecycle

foreign import data UseEffect :: Type -> Type

-- |
-- | performance issues, consider switching to `useEffect`.
useLifecycleEffect
  :: forall ps o m
   . EvalHookM ps o m (Maybe (EvalHookM ps o m Unit))
  -> Hook ps o m UseEffect Unit
useLifecycleEffect fn = Hooked $ Indexed $ liftF $ UseEffect Nothing fn unit

-- | Produces a hook for running post-render effects like subscriptions, timers,
-- | logging, and more. This replaces the usual Halogen `Initialize` and
-- | `Finalize` actions and extends them with the ability to run the same effect
-- | on each render so you don't have to manually call the effect after modifying
-- | state.
-- |
-- | The provided effect can return another effect to run as the finalizer.
-- |
-- | If the provided array of memo values is empty then the effect will run once
-- | at component mount and, if there is a provided finalizer, once at component
-- | unmount. If memo values are provided then the effect will also run on any
-- | render in which one of the memo values has changed.
-- |
-- | To run only on initialize and finalize:
-- |
-- | ```purs
-- | Hook.useEffect [] do
-- |   ...
-- | ```
-- |
-- | To run on initialize, finalize, and when a particular memo value has changed:
-- |
-- | ```purs
-- | Hook.captures { memoA, memoB } Hook.useEffect do
-- |   ...
-- | ```
useTickEffect
  :: forall ps o m
   . MemoValues
  -> EvalHookM ps o m (Maybe (EvalHookM ps o m Unit))
  -> Hook ps o m UseEffect Unit
useTickEffect memos fn = Hooked $ Indexed $ liftF $ UseEffect (Just memos) fn unit

-- Captures / Memo values

-- | Used to improve performance for hooks which may be expensive to run on
-- | many renders (like `useTickEffect` and `useMemo`). Uses a value equality
-- | check to verify values have changed before re-running a function.
-- |
-- | Some values may be expensive to check for value equality. If you only want
-- | to check referential equality or check a subpart of your captured values
-- | for value equality, please see `capturesWith`.
captures :: forall memos a. Eq (Record memos) => Record memos -> (MemoValues -> a) -> a
captures memos fn = fn (toMemoValues { eq: (==), memos })

-- | Like `captures`, but without an `Eq` constraint. Use with other equality
-- | checks when you only want to check equality for part of a memoized value
-- | or when you only want to verify referential equality.
-- |
-- | ```purs
-- | -- This function is used to implement the usual `captures`
-- | Hooks.captures { memoA, memoB } == Hooks.capturesWith eq { memoA }
-- |
-- | -- You can choose to test referential equality for efficiency (this is
-- | -- unsafe and can fire in unexpected ways, so be careful)
-- | Hooks.captures unsafeRefEq {}
-- |
-- | -- You can also choose to improve performance by testing only a sub-part
-- | -- of your memoized values. Remember that this equality check is used to
-- | -- decide whether to re-run your effect or function, so make sure to test
-- | -- everything in your captures list.
-- | let
-- |   customEq memoA memoB =
-- |     memoA.user.id == memoB.user.id
-- |       && memoA.data == memoB.data
-- |
-- | Hooks.capturesWith customEq { user, data }
-- | ```
capturesWith
  :: forall memos a
   . (Record memos -> Record memos -> Boolean)
  -> Record memos
  -> (MemoValues -> a)
  -> a
capturesWith memosEq memos fn = fn (toMemoValues { eq: memosEq, memos })

-- Coercing hooks

-- | Use when you want to turn a stack of hooks into a new custom hook type.
coerce :: forall hooks h' h ps o m a. Hooked ps o m hooks h' a -> Hooked ps o m hooks h a
coerce = unsafeCoerce

data InterpretHookReason
  = Initialize
  | Step
  | Finalize

component
  :: forall hooks i ps o m
   . (i -> Hooked ps o m Unit hooks (H.ComponentHTML (EvalHookM ps o m Unit) ps m))
  -> (forall q. H.Component HH.HTML q i o m)
component hookFn = componentWithQuery (\_ i -> hookFn i)

componentWithQuery
  :: forall hooks q i ps o m
   . (QueryToken q -> i -> Hooked ps o m Unit hooks (H.ComponentHTML (EvalHookM ps o m Unit) ps m))
  -> H.Component HH.HTML q i o m
componentWithQuery inputHookFn = do
  let
    hookFn = inputHookFn (unsafeCoerce unit :: QueryToken q)

  H.mkComponent
    { initialState
    , render: _.html
    , eval: case _ of
        H.Initialize a -> Prelude.do
          interpretHookFn Initialize hookFn
          interpretHookFn Step hookFn
          Prelude.pure a

        H.Query query reply -> Prelude.do
          { queryFn } <- H.get
          case queryFn of
            Nothing ->
              Prelude.pure (reply unit)
            Just fn -> Prelude.do
              let runHooks = interpretHookFn Step hookFn
              evalM runHooks $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) query

        H.Action act a -> Prelude.do
          evalM (interpretHookFn Step hookFn) act
          Prelude.pure a

        H.Receive input a -> Prelude.do
          H.modify_ _ { input = input }
          interpretHookFn Step hookFn
          Prelude.pure a

        H.Finalize a -> Prelude.do
          interpretHookFn Finalize hookFn
          Prelude.pure a
    }
  where
  initialState :: i -> HookState q i ps o m
  initialState input =
    { html: HH.text ""
    , stateCells: { queue: [], index: 0 }
    , memoCells: { queue: [], index: 0 }
    , input
    , queryFn: Nothing
    , finalizerFn: Nothing
    }

interpretHookFn
  :: forall hooks q i ps o m
   . InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks (H.ComponentHTML (EvalHookM ps o m Unit) ps m))
  -> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m Unit
interpretHookFn reason hookFn = Prelude.do
  { input } <- H.get
  let Hooked (Indexed hookF) = hookFn input
  html <- foldFree interpretHook hookF
  H.modify_ _ { html = html }
  where
  interpretHook :: HookF ps o m ~> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m
  interpretHook = case _ of
    UseState initial reply ->
      case reason of
        Initialize -> Prelude.do
          { stateCells: { queue } } <- H.get

          let
            newQueue = Array.snoc queue initial
            stateToken = StateToken (StateId (Array.length newQueue - 1))

          H.modify_ _ { stateCells { queue = newQueue } }
          Prelude.pure $ reply { getState: initial, stateToken }

        _ -> Prelude.do
          { stateCells: { index, queue } } <- H.get

          let
            stateValue = unsafeGetState (StateId index) queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            stateToken = StateToken (StateId index)

          H.modify_ _ { stateCells { index = nextIndex } }
          Prelude.pure $ reply { getState: stateValue, stateToken }

    UseQuery _ handler a -> Prelude.do
      let
        handler' :: forall a. q a -> EvalHookM ps o m (Maybe a)
        handler' = handler <<< toQueryValue

      H.modify_ _ { queryFn = Just (toQueryFn handler') }
      Prelude.pure a

    UseEffect mbMemos act a -> Prelude.do
      case reason of
        Initialize -> Prelude.do
          for_ mbMemos \memos -> Prelude.do
            { memoCells: { queue } } <- H.get
            H.modify_ _ { memoCells { queue = Array.snoc queue memos } }

          finalizer <- evalM mempty act
          when (isJust finalizer) Prelude.do
            H.modify_ _ { finalizerFn = finalizer }

        Step ->
          for_ mbMemos \memos -> Prelude.do
            { memoCells: { index, queue } } <- H.get

            let
              newQueue = unsafeSetMemos (MemoId index) memos queue
              nextIndex = if index + 1 < Array.length queue then index + 1 else 0

              oldMemos :: forall memos. MemoValues' memos
              oldMemos = fromMemoValues (unsafeGetMemos (MemoId index) queue)

              newMemos :: forall memos. MemoValues' memos
              newMemos = fromMemoValues memos

            H.modify_ _ { memoCells = { index: nextIndex, queue: newQueue } }

            when (newMemos.memos == {} || not newMemos.eq oldMemos newMemos) Prelude.do
              void $ evalM mempty act

        Finalize -> Prelude.do
          { finalizerFn } <- H.get
          for_ finalizerFn (evalM mempty)

      Prelude.pure a
