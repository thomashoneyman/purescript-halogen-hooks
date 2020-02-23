module Halogen.Hook
  ( useState
  , UseState
  , Hook
  , Hooked
  , component
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
import Data.Functor.Indexed (class IxFunctor)
import Data.Indexed (Indexed(..))
import Data.Tuple.Nested ((/\), type (/\))
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Functor, type (~>), Unit, map, unit, ($), (+), (<))
import Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | The Hook API: a set of primitive building blocks that can be used on their
-- | own to share stateful logic or used to create new hooks.
data HookF o m a
  = UseState StateValue (StateInterface -> a)
  | UseInitializer (EvalHookM o m Unit) a
  | UseFinalizer (EvalHookM o m Unit) a

derive instance functorHookF :: Functor (HookF o m)

newtype Hooked o m pre post a = Hooked (Indexed (Free (HookF o m)) pre post a)

derive newtype instance ixFunctorIndexed :: IxFunctor (Hooked o m)
derive newtype instance ixApplyIndexed :: IxApply (Hooked o m)
derive newtype instance ixApplicativeIndexed :: IxApplicative (Hooked o m)
derive newtype instance ixBindIndexed :: IxBind (Hooked o m)
derive newtype instance ixMonadIndexed :: IxMonad (Hooked o m)

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

-- | Exported for use with qualified-do syntax
pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

type Hook o m (newHook :: Type -> Type) a
  = forall hooks. Hooked o m hooks (newHook hooks) a

--  State

foreign import data UseState :: Type -> Type -> Type

type StateInterface =
  { stateToken :: StateToken StateValue
  , getState :: StateValue
  }

useState
  :: forall state o m
   . state
  -> Hook o m (UseState state) (state /\ StateToken state)
useState initialState = Hooked $ Indexed $ liftF $ UseState initialStateValue hookInterface
  where
  initialStateValue :: StateValue
  initialStateValue = toStateValue initialState

  hookInterface :: StateInterface -> state /\ StateToken state
  hookInterface { getState, stateToken } = fromStateValue getState /\ unsafeCoerce stateToken

data InterpretHookReason
  = Initialize
  | Step
  | Finalize

component
  :: forall hooks slots q i o m
   . (i -> Hooked o m Unit hooks (H.ComponentHTML (EvalHookM o m Unit) slots m))
  -> H.Component HH.HTML q i o m
component hookFn' = do
  let
    -- Hide the slot value internally.
    hookFn :: i -> Hooked o m Unit hooks (H.ComponentHTML (EvalHookM o m Unit) SlotValue m)
    hookFn i = do
      let (Hooked (Indexed x)) = hookFn' i
      Hooked $ Indexed $ map hideSlotsComponentHTML x

  H.mkComponent
    { initialState
    , render: _.html
    , eval: case _ of
        H.Initialize a -> Prelude.do
          runInterpreter Initialize hookFn
          Prelude.pure a

        H.Query query reply -> Prelude.do
          -- runInterpreter (Query query reply) hookFn
          Prelude.pure (reply unit)

        H.Action (EvalHookM act) a -> Prelude.do
          -- TODO: It's necessary to pass the hook interpreter to the evalHook
          -- interpreter so that state updates can always cause a re-render in
          -- the html in state.

          { input } <- H.get
          let Hooked (Indexed hookF) = hookFn input

          foldFree interpretEvalHook act
          html <- foldFree (interpretHook Step hookFn) hookF

          H.modify_ _ { html = html }
          Prelude.pure a

        H.Receive input a -> Prelude.do
          H.modify_ _ { input = input }
          runInterpreter Step hookFn
          Prelude.pure a

        H.Finalize a -> Prelude.do
          runInterpreter Finalize hookFn
          Prelude.pure a
    }
  where
  initialState :: i -> HookState i o m
  initialState input =
    { html: HH.text ""
    , state: { queue: [], total: 0, index: 0 }
    , input
    }

runInterpreter
  :: forall i o m hooks
   . InterpretHookReason
  -> (i -> Hooked o m Unit hooks (H.ComponentHTML (EvalHookM o m Unit) SlotValue m))
  -> H.HalogenM (HookState i o m) (EvalHookM o m Unit) SlotValue o m Unit
runInterpreter reason hookFn = Prelude.do
  { input } <- H.get

  let
    Hooked (Indexed hookF) = hookFn input
    interpret = interpretHook reason hookFn

  html <- foldFree interpret hookF
  H.modify_ _ { html = html }
  Prelude.pure unit

interpretHook
  :: forall i o m hooks
   . InterpretHookReason
  -> (i -> Hooked o m Unit hooks (H.ComponentHTML (EvalHookM o m Unit) SlotValue m))
  -> HookF o m
  ~> H.HalogenM (HookState i o m) (EvalHookM o m Unit) SlotValue o m
interpretHook reason hookFn = case _ of
  UseState initial reply ->
    case reason of
      Initialize -> Prelude.do
        { id } <- initializeHook initial
        Prelude.pure $ reply { getState: initial, stateToken: StateToken id }

      _ -> Prelude.do
        { state, id } <- stepHook
        Prelude.pure $ reply { getState: state, stateToken: StateToken id }

  UseInitializer (EvalHookM act) a -> Prelude.do
    case reason of
      Initialize -> Prelude.do
        foldFree interpretEvalHook act
      _ ->
        Prelude.pure unit

    Prelude.pure a

  UseFinalizer (EvalHookM act) a -> Prelude.do
    case reason of
      Finalize -> Prelude.do
        foldFree interpretEvalHook act
      _ ->
        Prelude.pure unit

    Prelude.pure a


initializeHook :: forall i a s o m. StateValue -> H.HalogenM (HookState i o m) a s o m { id :: StateId }
initializeHook initialState = Prelude.do
  { state } <- H.get

  let
    queue =
      { queue: Array.snoc state.queue initialState
      , index: 0
      , total: state.total + 1
      }

  H.modify_ _ { state = queue }
  Prelude.pure { id: StateId state.total }

stepHook :: forall i a s o m. H.HalogenM (HookState i o m) a s o m { state :: StateValue, id :: StateId }
stepHook = Prelude.do
  { state } <- H.get

  let
    stateValue = unsafeGetState (StateId state.index) state.queue
    nextIndex =
      if state.index + 1 < state.total
        then state.index + 1
        else 0

    queue =
      { queue: state.queue
      , index: nextIndex
      , total: state.total
      }

  H.modify_ _ { state = queue }
  Prelude.pure { state: stateValue, id: StateId state.index }
