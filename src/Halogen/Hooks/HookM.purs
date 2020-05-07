-- | A replacement for `Halogen.Query.HalogenM` which supports a near-identical
-- | API, but adjusted for compatibility with hooks. Most functions typically
-- | available in `HalogenM` are still available here, but some have modified
-- | behavior (for example, the state functions `get`, `put`, and `modify` don't
-- | exist; instead, the `useState` hook returns a `modify` function you can use).
module Halogen.Hooks.HookM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader (class MonadAsk, class MonadTrans, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell, tell)
import Control.Parallel (class Parallel)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Hooks.Internal.Types (OutputValue, SlotType, StateToken, StateValue, toOutputValue)
import Halogen.Hooks.Types (OutputToken, SlotToken)
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM
import Web.HTML as HTML
import Web.HTML.HTMLElement as HTMLElement

-- | A DSL compatible with HalogenM which is used to write effectful code
-- | for Hooks.
data HookF m a
  = Modify (StateToken StateValue) (StateValue -> StateValue) a
  | Subscribe (H.SubscriptionId -> ES.EventSource m (HookM m Unit)) (H.SubscriptionId -> a)
  | Unsubscribe H.SubscriptionId a
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox SlotType a)
  | Raise OutputValue a
  | Par (HookAp m a)
  | Fork (HookM m Unit) (H.ForkId -> a)
  | Kill H.ForkId a
  | GetRef H.RefLabel (Maybe DOM.Element -> a)

derive instance functorHookF :: Functor m => Functor (HookF m)

-- | The Hook effect monad, used to write effectful code in Hooks functions.
-- | This monad is fully compatible with `HalogenM`. meaning all functionality
-- | available for `HalogenM` is available in `HookM`.
newtype HookM m a = HookM (Free (HookF m) a)

derive newtype instance functorHookM :: Functor (HookM m)
derive newtype instance applyHookM :: Apply (HookM m)
derive newtype instance applicativeHookM :: Applicative (HookM m)
derive newtype instance bindHookM :: Bind (HookM m)
derive newtype instance monadHookM :: Monad (HookM m)
derive newtype instance semigroupHookM :: Semigroup a => Semigroup (HookM m a)
derive newtype instance monoidHookM :: Monoid a => Monoid (HookM m a)

instance monadEffectHookM :: MonadEffect m => MonadEffect (HookM m) where
  liftEffect = HookM <<< liftF <<< Lift <<< liftEffect

instance monadAffHookM :: MonadAff m => MonadAff (HookM m) where
  liftAff = HookM <<< liftF <<< Lift <<< liftAff

instance monadTransHookM :: MonadTrans HookM where
  lift = HookM <<< liftF <<< Lift

instance monadRecHookM :: MonadRec (HookM m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance monadAskHookM :: MonadAsk r m => MonadAsk r (HookM m) where
  ask = HookM $ liftF $ Lift ask

instance monadTellHookM :: MonadTell w m => MonadTell w (HookM m) where
  tell = HookM <<< liftF <<< Lift <<< tell

instance monadThrowHookM :: MonadThrow e m => MonadThrow e (HookM m) where
  throwError = HookM <<< liftF <<< Lift <<< throwError

-- | An applicative-only version of `HookM` to allow for parallel evaluation.
newtype HookAp m a = HookAp (FreeAp (HookM m) a)

derive instance newtypeHookAp :: Newtype (HookAp m a) _
derive newtype instance functorHookAp :: Functor (HookAp m)
derive newtype instance applyHookAp :: Apply (HookAp m)
derive newtype instance applicativeHookAp :: Applicative (HookAp m)

instance parallelHookM :: Parallel (HookAp m) (HookM m) where
  parallel = HookAp <<< liftFreeAp
  sequential = HookM <<< liftF <<< Par

-- | Raise an output message for the component. Requires a token carrying the
-- | output type of the component, which is provided by the `Hooks.component`
-- | function.
raise :: forall o m. OutputToken o -> o -> HookM m Unit
raise _ o = HookM $ liftF $ Raise (toOutputValue o) unit

-- | Send a query to a child of a component at the specified slot. Requires a
-- | token carrying the slot type of the component, which is provided by the
-- | `Hooks.component` function.
query
  :: forall m label ps query o' slot a _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SlotToken ps
  -> SProxy label
  -> slot
  -> query a
  -> HookM m (Maybe a)
query token label p q =
  HookM $ liftF $ ChildQuery $ box $ CQ.mkChildQueryBox do
    CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label p) q identity
  where
  box :: CQ.ChildQueryBox ps ~> CQ.ChildQueryBox SlotType
  box = unsafeCoerce

-- | Send a query to all children of a component at the specified slot. Requires
-- | a token carrying the slot type of the component, which is provided by the
-- | `Hooks.component` function.
queryAll
  :: forall m label ps query o' slot a _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SlotToken ps
  -> SProxy label
  -> query a
  -> HookM m (Map slot a)
queryAll token label q =
  HookM $ liftF $ ChildQuery $ box $ CQ.mkChildQueryBox do
    CQ.ChildQuery (\k -> map catMapMaybes <<< traverse k <<< Slot.slots label) q identity
  where
  box :: CQ.ChildQueryBox ps ~> CQ.ChildQueryBox SlotType
  box = unsafeCoerce

  catMapMaybes :: forall k v. Ord k => Map k (Maybe v) -> Map k v
  catMapMaybes = foldrWithIndex (\k v acc -> maybe acc (flip (Map.insert k) acc) v) Map.empty

-- | Subscribes a component to an `EventSource`. When a component is disposed of
-- | any active subscriptions will automatically be stopped and no further subscriptions
-- | will be possible during finalization.
subscribe :: forall m. ES.EventSource m (HookM m Unit) -> HookM m H.SubscriptionId
subscribe es = HookM $ liftF $ Subscribe (\_ -> es) identity

-- | An alternative to `subscribe`, intended for subscriptions that unsubscribe
-- | themselves. Instead of returning the `SubscriptionId` from `subscribe'`, it
-- | is passed into an `EventSource` constructor. This allows emitted queries
-- | to include the `SubscriptionId`, rather than storing it in the state of the
-- | component.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe' :: forall m. (H.SubscriptionId -> ES.EventSource m (HookM m Unit)) -> HookM m Unit
subscribe' esc = HookM $ liftF $ Subscribe esc (const unit)

-- | Unsubscribes a component from an `EventSource`. If the subscription
-- | associated with the ID has already ended this will have no effect.
unsubscribe :: forall m. H.SubscriptionId -> HookM m Unit
unsubscribe sid = HookM $ liftF $ Unsubscribe sid unit

-- | Starts a `HalogenM` process running independent from the current `eval`
-- | "thread".
-- |
-- | A commonly use case for `fork` is in component initializers where some
-- | async action is started. Normally all interaction with the component will
-- | be blocked until the initializer completes, but if the async action is
-- | `fork`ed instead, the initializer can complete synchronously while the
-- | async action continues.
-- |
-- | Some care needs to be taken when using a `fork` that can modify the
-- | component state, as it's easy for the forked process to "clobber" the state
-- | (overwrite some or all of it with an old value) by mistake.
-- |
-- | When a component is disposed of any active forks will automatically
-- | be killed. New forks can be started during finalization but there will be
-- | no means of killing them.
fork :: forall m. HookM m Unit -> HookM m H.ForkId
fork fn = HookM $ liftF $ Fork fn identity

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall m. H.ForkId -> HookM m Unit
kill fid = HookM $ liftF $ Kill fid unit

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered o of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall m. H.RefLabel -> HookM m (Maybe DOM.Element)
getRef p = HookM $ liftF $ GetRef p identity

-- | Retrieves a `HTMLElement` value that is associated with a `Ref` in the
-- | rendered o of a component. If there is no currently rendered value (or
-- | it is not an `HTMLElement`) for the request will return `Nothing`.
getHTMLElementRef :: forall m. H.RefLabel -> HookM m (Maybe HTML.HTMLElement)
getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
