module Halogen.EvalHookM where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

-- | The EvalHook API: a set of primitive building blocks that can be used as
-- | an alternate interface to HalogenM when evaluating hooks. Implemented so
-- | that multiple states can be accessed by different hooks.
data EvalHookF slots output m a
  = Modify (StateToken StateValue) (StateValue -> StateValue) (StateValue -> a)
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox slots a)
  | Raise output a

derive instance functorHookF :: Functor m => Functor (EvalHookF slots output m)

-- | The Hook effect monad, an interface to the HalogenM component eval effect monad
newtype EvalHookM slots output m a = EvalHookM (Free (EvalHookF slots output m) a)

derive newtype instance functorEvalHookM :: Functor (EvalHookM slots output m)
derive newtype instance applyEvalHookM :: Apply (EvalHookM slots output m)
derive newtype instance applicativeEvalHookM :: Applicative (EvalHookM slots output m)
derive newtype instance bindEvalHookM :: Bind (EvalHookM slots output m)
derive newtype instance monadEvalHookM :: Monad (EvalHookM slots output m)
derive newtype instance semigroupEvalHookM :: Semigroup a => Semigroup (EvalHookM slots output m a)
derive newtype instance monoidEvalHookM :: Monoid a => Monoid (EvalHookM slots output m a)

instance monadEffectEvalHookM :: MonadEffect m => MonadEffect (EvalHookM slots output m) where
  liftEffect = EvalHookM <<< liftF <<< Lift <<< liftEffect

instance monadAffEvalHookM :: MonadAff m => MonadAff (EvalHookM slots output m) where
  liftAff = EvalHookM <<< liftF <<< Lift <<< liftAff

-- Query

foreign import data QueryToken :: (Type -> Type) -> Type

-- State

foreign import data StateValue :: Type

toStateValue :: forall state. state -> StateValue
toStateValue = unsafeCoerce

fromStateValue :: forall state. StateValue -> state
fromStateValue = unsafeCoerce

-- Used to uniquely identify a cell in state as well as its type so it can be
-- modified safely by users but is also available in a heterogeneous collection
-- in component state. Should not have its constructor exported.
newtype StateToken state = StateToken StateId

get :: forall state slots output m. StateToken state -> EvalHookM slots output m state
get token = modify token identity

put :: forall state slots output m. StateToken state -> state -> EvalHookM slots output m Unit
put token state = modify_ token (const state)

modify_ :: forall state slots output m. StateToken state -> (state -> state) -> EvalHookM slots output m Unit
modify_ token = map (const unit) <<< modify token

modify :: forall state slots output m. StateToken state -> (state -> state) -> EvalHookM slots output m state
modify token f = EvalHookM $ liftF $ Modify token' f' state
  where
  token' :: StateToken StateValue
  token' = unsafeCoerce token

  f' :: StateValue -> StateValue
  f' = toStateValue <<< f <<< fromStateValue

  state :: StateValue -> state
  state = fromStateValue

-- Outputs

raise :: forall slots output m. output -> EvalHookM slots output m Unit
raise output = EvalHookM $ liftF $ Raise output unit

-- Querying
query
  :: forall output m label slots query output' slot a _1
   . Row.Cons label (H.Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> query a
  -> EvalHookM slots output m (Maybe a)
query label p q = EvalHookM $ liftF $ ChildQuery $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k → maybe (pure Nothing) k <<< Slot.lookup label p) q identity

-- Interpreter

foreign import data QueryFn :: (Type -> Type) -> # Type -> Type -> (Type -> Type) -> Type

toQueryFn :: forall q ps o m. (forall a. q a -> EvalHookM ps o m (Maybe a)) -> QueryFn q ps o m
toQueryFn = unsafeCoerce

fromQueryFn :: forall q ps o m. QueryFn q ps o m -> (forall a. q a -> EvalHookM ps o m (Maybe a))
fromQueryFn = unsafeCoerce

type HookState q i ps o m =
  { state :: QueueState
  , html :: H.ComponentHTML (EvalHookM ps o m Unit) ps m
  , input :: i
  , queryFn :: Maybe (QueryFn q ps o m)
  }

type QueueState =
  { queue :: Array StateValue
  , total :: Int
  , index :: Int
  }

newtype StateId = StateId Int

derive newtype instance eqStateId :: Eq StateId
derive newtype instance ordStateId :: Ord StateId
derive newtype instance showStateId :: Show StateId

interpretEvalHook :: forall q i a ps o m. EvalHookF ps o m ~> H.HalogenM (HookState q i ps o m) a ps o m
interpretEvalHook = case _ of
  Modify (StateToken token) f reply -> do
    state <- H.get

    let
      v = unsafeGetState token state.state.queue
      v' = f v

    H.put $ state { state { queue = unsafeSetState token v' state.state.queue } }
    pure (reply v')

  Lift f -> H.HalogenM $ liftF $ H.Lift f

  ChildQuery box -> H.HalogenM $ liftF $ H.ChildQuery box

  Raise o a -> H.raise o *> pure a

-- Utilities for updating state

unsafeGetState :: StateId -> Array StateValue -> StateValue
unsafeGetState (StateId index) array = unsafePartial (Array.unsafeIndex array index)

unsafeSetState :: StateId -> StateValue -> Array StateValue -> Array StateValue
unsafeSetState (StateId index) a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))
