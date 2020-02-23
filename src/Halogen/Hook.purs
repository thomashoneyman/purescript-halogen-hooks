module Halogen.Hook
  ( useState
  , UseState
  , useQuery
  , UseQuery
  , useInitializer
  , UseInitializer
  , useFinalizer
  , UseFinalizer
  , Hook
  , Hooked
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
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Functor, type (~>), Unit, map, unit, ($), (+), (<), (<<<))
import Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | The Hook API: a set of primitive building blocks that can be used on their
-- | own to share stateful logic or used to create new hooks.
data HookF q ps o m a
  = UseState StateValue (StateInterface -> a)
  | UseQuery (QueryToken q) (forall b. q b -> EvalHookM ps o m (Maybe b)) a
  | UseInitializer (EvalHookM ps o m Unit) a
  | UseFinalizer (EvalHookM ps o m Unit) a

derive instance functorHookF :: Functor (HookF q ps o m)

newtype Hooked q ps o m pre post a = Hooked (Indexed (Free (HookF q ps o m)) pre post a)

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked q ps o m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked q ps o m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked q ps o m)
derive newtype instance ixBindIndexed :: IxBind (Hooked q ps o m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked q ps o m)

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

type Hook q ps o m (newHook :: Type -> Type) a
  = forall hooks. Hooked q ps o m hooks (newHook hooks) a

--  State

foreign import data UseState :: Type -> Type -> Type

type StateInterface =
  { stateToken :: StateToken StateValue
  , getState :: StateValue
  }

useState :: forall state q ps o m. state -> Hook q ps o m (UseState state) (state /\ StateToken state)
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
  -> Hook q ps o m UseQuery Unit
useQuery token handler = Hooked $ Indexed $ liftF $ UseQuery token handler unit

-- Lifecycle

foreign import data UseInitializer :: Type -> Type

useInitializer :: forall q ps o m. EvalHookM ps o m Unit -> Hook q ps o m UseInitializer Unit
useInitializer initializer = Hooked $ Indexed $ liftF $ UseInitializer initializer unit

foreign import data UseFinalizer :: Type -> Type

useFinalizer :: forall q ps o m. EvalHookM ps o m Unit -> Hook q ps o m UseFinalizer Unit
useFinalizer finalizer = Hooked $ Indexed $ liftF $ UseFinalizer finalizer unit

data InterpretHookReason
  = Initialize
  | Step
  | Finalize

component
  :: forall hooks i ps o m
   . (forall q. i -> Hooked q ps o m Unit hooks (H.ComponentHTML (EvalHookM ps o m Unit) ps m))
  -> (forall q. H.Component HH.HTML q i o m)
component hookFn = componentWithQuery (\_ i -> hookFn i)

componentWithQuery
  :: forall hooks q i ps o m
   . (QueryToken q -> i -> Hooked q ps o m Unit hooks (H.ComponentHTML (EvalHookM ps o m Unit) ps m))
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
          Prelude.pure a

        H.Query query reply -> Prelude.do
          { queryFn } <- H.get
          case queryFn of
            Nothing ->
              Prelude.pure (reply unit)
            Just fn -> Prelude.do
              let (EvalHookM eval) = unCoyoneda (\g -> map (maybe (reply unit) g) <<< (fromQueryFn fn)) query
              foldFree interpretEvalHook eval

        H.Action (EvalHookM act) a -> Prelude.do
          -- TODO: It's necessary to pass the hook interpreter to the evalHook
          -- interpreter so that state updates can always cause a re-render in
          -- the html in state.
          foldFree interpretEvalHook act
          interpretHookFn Step hookFn
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
    , state: { queue: [], total: 0, index: 0 }
    , input
    , queryFn: Nothing
    }

  interpretHookFn reason hookFn = Prelude.do
    { input } <- H.get
    let Hooked (Indexed hookF) = hookFn input
    html <- foldFree (interpretHook reason) hookF
    H.modify_ _ { html = html }
    Prelude.pure unit

interpretHook
  :: forall q i ps o m
   . InterpretHookReason
  -> HookF q ps o m
  ~> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m
interpretHook reason = case _ of
  UseState initial reply ->
    case reason of
      Initialize -> Prelude.do
        { state } <- H.get

        let
          newState =
            { queue: Array.snoc state.queue initial
            , index: 0
            , total: state.total + 1
            }

        H.modify_ _ { state = newState }
        Prelude.pure $ reply { getState: initial, stateToken: StateToken (StateId state.total) }

      _ -> Prelude.do
        { state } <- H.get

        let
          stateValue = unsafeGetState (StateId state.index) state.queue
          nextIndex = if state.index + 1 < state.total then state.index + 1 else 0
          newState =
            { queue: state.queue
            , index: nextIndex
            , total: state.total
            }

        H.modify_ _ { state = newState }
        Prelude.pure $ reply { getState: stateValue, stateToken: StateToken (StateId state.index) }

  UseQuery _ handler a -> Prelude.do
    H.modify_ _ { queryFn = Just (toQueryFn handler) }
    Prelude.pure a

  UseInitializer (EvalHookM act) a -> Prelude.do
    case reason of
      Initialize -> foldFree interpretEvalHook act
      _ -> Prelude.pure unit
    Prelude.pure a

  UseFinalizer (EvalHookM act) a -> Prelude.do
    case reason of
      Finalize -> foldFree interpretEvalHook act
      _ -> Prelude.pure unit
    Prelude.pure a
