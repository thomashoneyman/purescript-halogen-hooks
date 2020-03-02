module Halogen.Hook
  ( useState
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
import Data.Bifunctor (bimap)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (for_, sequence_)
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen (liftEffect)
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
  | UseMemo MemoValues (Unit -> MemoValue) (MemoValue -> a)
  | UseRef RefValue (RefInterface -> a)

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

-- | Produces a hook for running an effect on component Initialize, which can
-- | return an effect to run on component Finalize. If you would like to run
-- | your effect after every render, see `useTickEffect`.
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
-- | To run on every render:
-- |
-- | ```purs
-- | Hook.captures {} Hook.useTickEffect do
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

-- Memoization

foreign import data UseMemo :: Type -> Type -> Type

-- | When using values in let bindings within the body of a hook they will be
-- | recomputed each time the hooks body is evaluated (on render). You can use
-- | `useMemo` to prevent values from being recomputed unless a dependency has
-- | changed (as provided via `captures`).
-- |
-- | Be careful to include any arguments to the function or other dependencies
-- | which should cause the function to be re-run; if omitted you will end up
-- | with stale state.
-- |
-- | If you provide an empty set of memo values then this function will have no
-- | effect and no memoization will take place.
useMemo :: forall ps o m a. MemoValues -> (Unit -> a) -> Hook ps o m (UseMemo a) a
useMemo memos fn = Hooked $ Indexed $ liftF $ UseMemo memos to from
  where
  to :: Unit -> MemoValue
  to = toMemoValue <<< fn

  from :: MemoValue -> a
  from = fromMemoValue

-- Captures for memo values

-- | Used to improve performance for hooks which may be expensive to run on
-- | many renders (like `useTickEffect` and `useMemo`). Uses a value equality
-- | check to verify values have changed before re-running a function.
-- |
-- | Some values may be expensive to check for value equality. You can optimize
-- | this by only checking a sub-part of your captured values using `capturesWith`
captures :: forall memos a. Eq (Record memos) => Record memos -> (MemoValues -> a) -> a
captures memos fn = fn (toMemoValues { eq: eq', memos: toObject memos })
  where
  -- This coercion from a record to an object and back relies on the representation
  -- of records as objects in generated JavaScript, but it really ought to be
  -- implemented via the FFI in the future to preserve compatibility with other
  -- backends and to guard against potential changes to PureScript code generation.
  toObject :: Record memos -> Object MemoValue
  toObject = unsafeCoerce

  toRecord :: Object MemoValue -> Record memos
  toRecord = unsafeCoerce

  eq' :: Object MemoValue -> Object MemoValue -> Boolean
  eq' a b = toRecord a == toRecord b

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
capturesWith memosEq memos fn = fn (toMemoValues { eq: eq', memos: toObject memos })
  where
  toObject :: Record memos -> Object MemoValue
  toObject = unsafeCoerce

  toRecord :: Object MemoValue -> Record memos
  toRecord = unsafeCoerce

  eq' :: Object MemoValue -> Object MemoValue -> Boolean
  eq' a b = toRecord a `memosEq` toRecord b

-- Refs

foreign import data UseRef :: Type -> Type -> Type

type RefInterface =
  { ref :: Ref RefValue
  , value :: RefValue
  }

useRef :: forall a ps o m. a -> Hook ps o m (UseRef a) (a /\ Ref a)
useRef initialValue = Hooked $ Indexed $ liftF $ UseRef initialValue' interface
  where
  initialValue' :: RefValue
  initialValue' = toRefValue initialValue

  interface :: RefInterface -> a /\ Ref a
  interface { ref, value } = fromRefValue value /\ (unsafeCoerce :: Ref RefValue -> Ref a) ref

-- Coercing hooks

-- | Use when you want to turn a stack of hooks into a new custom hook type.
coerce :: forall hooks h' h ps o m a. Hooked ps o m hooks h' a -> Hooked ps o m hooks h a
coerce = unsafeCoerce

data InterpretHookReason
  = Initialize
  | Queued
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
    , render: \(HookState { html }) -> html
    , eval: case _ of
        H.Initialize a -> Prelude.do
          interpretHookFn Initialize hookFn
          runEvalQueue
          Prelude.pure a

        H.Query query reply -> Prelude.do
          HookState { queryFn } <- H.get
          case queryFn of
            Nothing ->
              Prelude.pure (reply unit)
            Just fn -> Prelude.do
              let
                runHooks = Prelude.do
                  interpretHookFn Step hookFn
                  runEvalQueue
              evalM runHooks $ unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) query

        H.Action act a -> Prelude.do
          evalM (interpretHookFn Step hookFn) act
          runEvalQueue
          Prelude.pure a

        H.Receive input a -> Prelude.do
          H.modify_ (over HookState _ { input = input })
          interpretHookFn Step hookFn
          runEvalQueue
          Prelude.pure a

        H.Finalize a -> Prelude.do
          interpretHookFn Finalize hookFn
          runEvalQueue
          Prelude.pure a
    }
  where
  initialState :: i -> HookState q i ps o m
  initialState input = HookState
    { input
    , html: HH.text ""
    , queryFn: Nothing
    , stateCells: { queue: [], index: 0 }
    , effectCells: { queue: [], index: 0 }
    , memoCells: { queue: [], index: 0 }
    , refCells: { queue: [], index: 0 }
    , finalizerQueue: []
    , evalQueue: []
    }

-- When hooks are interpreted effects are queued. This queue needs to be processed
-- immediately after.
runEvalQueue :: forall q i ps o m. H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m Unit
runEvalQueue = Prelude.do
  HookState { evalQueue } <- H.get
  sequence_ evalQueue
  H.modify_ (over HookState _ { evalQueue = [] })

interpretHookFn
  :: forall hooks q i ps o m
   . InterpretHookReason
  -> (i -> Hooked ps o m Unit hooks (H.ComponentHTML (EvalHookM ps o m Unit) ps m))
  -> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m Unit
interpretHookFn reason hookFn = Prelude.do
  HookState { input } <- H.get
  let Hooked (Indexed hookF) = hookFn input
  html <- foldFree interpretHook hookF
  H.modify_ (over HookState _ { html = html })
  where
  interpretHook :: HookF ps o m ~> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m
  interpretHook = case _ of
    UseState initial reply ->
      case reason of
        Initialize -> Prelude.do
          HookState { stateCells: { queue } } <- H.get

          let
            newQueue = Array.snoc queue initial
            stateToken = StateToken (StateId (Array.length newQueue - 1))

          H.modify_ (over HookState _ { stateCells { queue = newQueue } })
          Prelude.pure $ reply { getState: initial, stateToken }

        _ -> Prelude.do
          HookState { stateCells: { index, queue } } <- H.get

          let
            stateValue = unsafeGetStateCell (StateId index) queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            stateToken = StateToken (StateId index)

          H.modify_ (over HookState _ { stateCells { index = nextIndex } })
          Prelude.pure $ reply { getState: stateValue, stateToken }

    UseQuery _ handler a ->
      case reason of
        Initialize -> Prelude.do
          let
            handler' :: forall a. q a -> EvalHookM ps o m (Maybe a)
            handler' = handler <<< toQueryValue

          H.modify_ (over HookState _ { queryFn = Just (toQueryFn handler') })
          Prelude.pure a

        _ ->
          Prelude.pure a

    UseEffect mbMemos act a -> Prelude.do
      case reason of
        Initialize -> Prelude.do
          for_ mbMemos \memos -> Prelude.do
            H.modify_ \(HookState st) ->
              HookState $ st { effectCells { queue = Array.snoc st.effectCells.queue memos } }

          let
            eval = Prelude.do
              mbFinalizer <- evalM (interpretHookFn Queued hookFn) act
              for_ mbFinalizer \finalizer ->
                H.modify_ \(HookState st) ->
                  HookState $ st { finalizerQueue = Array.snoc st.finalizerQueue finalizer }

          H.modify_ \(HookState st) -> HookState $ st { evalQueue = Array.snoc st.evalQueue eval }

        Queued ->
          Prelude.pure unit

        Step -> Prelude.do
          for_ mbMemos \memos -> Prelude.do
            HookState { effectCells: { index, queue } } <- H.get

            let
              newQueue = unsafeSetEffectCell (EffectId index) memos queue
              nextIndex = if index + 1 < Array.length queue then index + 1 else 0

              memos' :: { old :: MemoValuesImpl, new :: MemoValuesImpl }
              memos' =
                { old: fromMemoValues (unsafeGetEffectCell (EffectId index) queue)
                , new: fromMemoValues memos
                }

            H.modify_ (over HookState _ { effectCells = { index: nextIndex, queue: newQueue } })

            when (Object.isEmpty memos'.new.memos || not memos'.new.eq memos'.old.memos memos'.new.memos) Prelude.do
              let eval = void $ evalM (interpretHookFn Queued hookFn) act
              H.modify_ \(HookState st) -> HookState $ st { evalQueue = Array.snoc st.evalQueue eval }

        Finalize -> Prelude.do
          HookState { finalizerQueue } <- H.get
          let evalQueue = map (evalM mempty) finalizerQueue
          H.modify_ \(HookState st) -> HookState $ st { evalQueue = Prelude.append st.evalQueue evalQueue }

      Prelude.pure a

    UseMemo memos memoFn reply -> Prelude.do
      case reason of
        Initialize -> Prelude.do
          HookState { memoCells: { queue } } <- H.get

          let
            newValue = memoFn unit

          H.modify_ (over HookState _ { memoCells { queue = Array.snoc queue (Tuple memos newValue) } })
          Prelude.pure $ reply newValue

        _ -> Prelude.do
          HookState { memoCells: { index, queue } } <- H.get

          let
            m = Prelude.do
              let
                Tuple oldMemos oldValue = bimap fromMemoValues fromMemoValue (unsafeGetMemoCell (MemoId index) queue)
                newMemos = fromMemoValues memos

              { eq: newMemos.eq
              , old: oldMemos.memos
              , new: newMemos.memos
              , value: oldValue
              }

          if (Object.isEmpty m.new || not (m.new `m.eq` m.old)) then Prelude.do
            let
              nextIndex = if index + 1 < Array.length queue then index + 1 else 0
              newValue = memoFn unit
              newQueue = unsafeSetMemoCell (MemoId index) (Tuple memos newValue) queue

            H.modify_ (over HookState _ { memoCells = { index: nextIndex, queue: newQueue } })
            Prelude.pure $ reply newValue

          else
            Prelude.pure $ reply m.value

    UseRef initial reply ->
      case reason of
        Initialize -> Prelude.do
          HookState { refCells: { queue } } <- H.get

          let
            ref = unsafePerformEffect $ liftEffect $ Ref.new initial

          H.modify_ (over HookState _ { refCells { queue = Array.snoc queue ref } })
          Prelude.pure $ reply { value: initial, ref }

        _ -> Prelude.do
          HookState { refCells: { index, queue } } <- H.get

          let
            ref = unsafeGetRefCell (RefId index) queue
            nextIndex = if index + 1 < Array.length queue then index + 1 else 0
            value = unsafePerformEffect $ liftEffect $ Ref.read ref

          H.modify_ (over HookState _ { refCells { index = nextIndex } })
          Prelude.pure $ reply { value, ref }
