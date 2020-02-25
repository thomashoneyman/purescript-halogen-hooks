module Halogen.EvalHookM where

import Prelude

import Control.Applicative.Free (FreeAp, hoistFreeAp, retractFreeAp)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

-- | The EvalHook API: a set of primitive building blocks that can be used as
-- | an alternate interface to HalogenM when evaluating hooks. Implemented so
-- | that multiple states can be accessed by different hooks.
data EvalHookF slots output m a
  = Modify (StateToken StateValue) (StateValue -> StateValue) (StateValue -> a)
  | Subscribe (H.SubscriptionId -> ES.EventSource m (EvalHookM slots output m Unit)) (H.SubscriptionId -> a)
  | Unsubscribe H.SubscriptionId a
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox slots a)
  | Raise output a
  | Par (EvalHookAp slots output m a)
  | Fork (EvalHookM slots output m Unit) (H.ForkId -> a)
  | Kill H.ForkId a
  | GetRef H.RefLabel (Maybe Element -> a)

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

-- | An applicative-only version of `EvalHookM` to allow for parallel evaluation.
newtype EvalHookAp slots output m a = EvalHookAp (FreeAp (EvalHookM slots output m) a)

derive instance newtypeEvalHookAp :: Newtype (EvalHookAp slots output m a) _
derive newtype instance functorEvalHookAp :: Functor (EvalHookAp slots output m)
derive newtype instance applyEvalHookAp :: Apply (EvalHookAp slots output m)
derive newtype instance applicativeEvalHookAp :: Applicative (EvalHookAp slots output m)

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

-- Subscription

subscribe :: forall slots output m. ES.EventSource m (EvalHookM slots output m Unit) -> EvalHookM slots output m H.SubscriptionId
subscribe es = EvalHookM $ liftF $ Subscribe (\_ -> es) identity

subscribe' :: forall slots output m. (H.SubscriptionId -> ES.EventSource m (EvalHookM slots output m Unit)) -> EvalHookM slots output m Unit
subscribe' esc = EvalHookM $ liftF $ Subscribe esc (const unit)

unsubscribe :: forall slots output m. H.SubscriptionId -> EvalHookM slots output m Unit
unsubscribe sid = EvalHookM $ liftF $ Unsubscribe sid unit

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

evalM
  :: forall q i ps o m
   . H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m Unit
  -> EvalHookM ps o m
  ~> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m
evalM runHooks (EvalHookM evalHookF) = foldFree interpretEvalHook evalHookF
  where
  interpretEvalHook :: EvalHookF ps o m ~> H.HalogenM (HookState q i ps o m) (EvalHookM ps o m Unit) ps o m
  interpretEvalHook = case _ of
    Modify (StateToken token) f reply -> do
      state <- H.get
      let v = f (unsafeGetState token state.state.queue)
      H.put $ state { state { queue = unsafeSetState token v state.state.queue } }
      runHooks
      pure (reply v)

    Subscribe eventSource reply -> do
      H.HalogenM $ liftF $ H.Subscribe eventSource reply

    Unsubscribe sid a ->
      H.HalogenM $ liftF $ H.Unsubscribe sid a

    Lift f ->
      H.HalogenM $ liftF $ H.Lift f

    ChildQuery box ->
      H.HalogenM $ liftF $ H.ChildQuery box

    Raise o a ->
      H.raise o *> pure a

    Par (EvalHookAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM runHooks) p

    Fork hmu reply ->
      H.HalogenM $ liftF $ H.Fork (evalM runHooks hmu) reply

    Kill fid a ->
      H.HalogenM $ liftF $ H.Kill fid a

    GetRef p reply ->
      H.HalogenM $ liftF $ H.GetRef p reply

-- Utilities for updating state

unsafeGetState :: StateId -> Array StateValue -> StateValue
unsafeGetState (StateId index) array = unsafePartial (Array.unsafeIndex array index)

unsafeSetState :: StateId -> StateValue -> Array StateValue -> Array StateValue
unsafeSetState (StateId index) a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))
