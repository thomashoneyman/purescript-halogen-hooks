-- | A replacement for `Halogen.Query.HalogenM` which supports a near-identical
-- | API, but adjusted for compatibility with hooks. All functions typically
-- | available in `HalogenM` are still available here, but some have modified
-- | behavior (for example, the state functions `get`, `put`, and `modify` require
-- | an additional `StateToken` argument when used in Hooks).
-- |
-- | If you need to use a function usually available in HalogenM which is not
-- | available here, please file an issue.
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
import Halogen.Hooks.Internal.Types (StateValue, fromStateValue, toStateValue)
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM
import Web.HTML as HTML
import Web.HTML.HTMLElement as HTMLElement

-- | HookF is a DSL fully compatible with HalogenM. It is used to implement
-- | the Hook monad, HookM.
data HookF ps o m a
  = Modify (StateToken StateValue) (StateValue -> StateValue) (StateValue -> a)
  | Subscribe (H.SubscriptionId -> ES.EventSource m (HookM ps o m Unit)) (H.SubscriptionId -> a)
  | Unsubscribe H.SubscriptionId a
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox ps a)
  | Raise o a
  | Par (HookAp ps o m a)
  | Fork (HookM ps o m Unit) (H.ForkId -> a)
  | Kill H.ForkId a
  | GetRef H.RefLabel (Maybe DOM.Element -> a)

derive instance functorHookF :: Functor m => Functor (HookF ps o m)

-- | The Hook effect monad, an interface to the HalogenM component eval effect monad.
newtype HookM ps o m a = HookM (Free (HookF ps o m) a)

derive newtype instance functorHookM :: Functor (HookM ps o m)
derive newtype instance applyHookM :: Apply (HookM ps o m)
derive newtype instance applicativeHookM :: Applicative (HookM ps o m)
derive newtype instance bindHookM :: Bind (HookM ps o m)
derive newtype instance monadHookM :: Monad (HookM ps o m)
derive newtype instance semigroupHookM :: Semigroup a => Semigroup (HookM ps o m a)
derive newtype instance monoidHookM :: Monoid a => Monoid (HookM ps o m a)

instance monadEffectHookM :: MonadEffect m => MonadEffect (HookM ps o m) where
  liftEffect = HookM <<< liftF <<< Lift <<< liftEffect

instance monadAffHookM :: MonadAff m => MonadAff (HookM ps o m) where
  liftAff = HookM <<< liftF <<< Lift <<< liftAff

instance monadTransHookM :: MonadTrans (HookM ps o) where
  lift = HookM <<< liftF <<< Lift

instance monadRecHookM :: MonadRec (HookM ps o m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance monadAskHookM :: MonadAsk r m => MonadAsk r (HookM ps o m) where
  ask = HookM $ liftF $ Lift ask

instance monadTellHookM :: MonadTell w m => MonadTell w (HookM ps o m) where
  tell = HookM <<< liftF <<< Lift <<< tell

instance monadThrowHookM :: MonadThrow e m => MonadThrow e (HookM ps o m) where
  throwError = HookM <<< liftF <<< Lift <<< throwError

-- | An applicative-only version of `HookM` to allow for parallel evaluation.
newtype HookAp ps o m a = HookAp (FreeAp (HookM ps o m) a)

derive instance newtypeHookAp :: Newtype (HookAp ps o m a) _
derive newtype instance functorHookAp :: Functor (HookAp ps o m)
derive newtype instance applyHookAp :: Apply (HookAp ps o m)
derive newtype instance applicativeHookAp :: Applicative (HookAp ps o m)

instance parallelHookM :: Parallel (HookAp ps o m) (HookM ps o m) where
  parallel = HookAp <<< liftFreeAp
  sequential = HookM <<< liftF <<< Par

newtype StateToken state = StateToken Int

-- | Get a piece of state using a token received from the `useState` hook.
-- |
-- | ```purs
-- | _ /\ countState :: StateToken Int <- useState 0
-- |
-- | let
-- |   onClick = do
-- |     count :: Int <- get countState
-- |     ...
-- | ```
get :: forall state ps o m. StateToken state -> HookM ps o m state
get token = modify token identity

-- | Overwrite a piece of state using a token received from the `useState` hook.
-- |
-- | ```purs
-- | _ /\ countState :: StateToken Int <- useState 0
-- |
-- | let
-- |   onClick = do
-- |     put countState 10
-- | ```
put :: forall state ps o m. StateToken state -> state -> HookM ps o m Unit
put token state = modify_ token (const state)

-- | Modify a piece of state using a token received from the `useState` hook.
-- |
-- | ```purs
-- | _ /\ countState :: StateToken Int <- useState 0
-- |
-- | let
-- |   onClick = do
-- |     modify_ countState (_ + 10)
-- | ```
modify_ :: forall state ps o m. StateToken state -> (state -> state) -> HookM ps o m Unit
modify_ token = map (const unit) <<< modify token

-- | Modify a piece of state using a token received from the `useState` hook,
-- | returning the new state.
-- |
-- | ```purs
-- | _ /\ countState :: StateToken Int <- useState 0
-- |
-- | let
-- |   onClick = do
-- |     count :: Int <- modify countState (_ + 10)
-- |     ...
-- | ```
modify :: forall state ps o m. StateToken state -> (state -> state) -> HookM ps o m state
modify token f = HookM $ liftF $ Modify token' f' state
  where
  token' :: StateToken StateValue
  token' = unsafeCoerce token

  f' :: StateValue -> StateValue
  f' = toStateValue <<< f <<< fromStateValue

  state :: StateValue -> state
  state = fromStateValue

-- | Raise an output message for the component.
raise :: forall ps o m. o -> HookM ps o m Unit
raise o = HookM $ liftF $ Raise o unit

-- | Send a query to a child of a component at the specified slot
query
  :: forall o m label ps query o' slot a _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> query a
  -> HookM ps o m (Maybe a)
query label p q =
  HookM $ liftF $ ChildQuery $ CQ.mkChildQueryBox do
    CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label p) q identity

-- | Sends a query to all children of a component at a given slot label.
queryAll
  :: forall o m label ps query o' slot a _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> query a
  -> HookM ps o m (Map slot a)
queryAll label q =
  HookM $ liftF $ ChildQuery $ CQ.mkChildQueryBox $
    CQ.ChildQuery (\k -> map catMapMaybes <<< traverse k <<< Slot.slots label) q identity
  where
  catMapMaybes :: forall k v. Ord k => Map k (Maybe v) -> Map k v
  catMapMaybes = foldrWithIndex (\k v acc -> maybe acc (flip (Map.insert k) acc) v) Map.empty

-- | Subscribes a component to an `EventSource`. When a component is disposed of
-- | any active subscriptions will automatically be stopped and no further subscriptions
-- | will be possible during finalization.
subscribe :: forall ps o m. ES.EventSource m (HookM ps o m Unit) -> HookM ps o m H.SubscriptionId
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
subscribe' :: forall ps o m. (H.SubscriptionId -> ES.EventSource m (HookM ps o m Unit)) -> HookM ps o m Unit
subscribe' esc = HookM $ liftF $ Subscribe esc (const unit)

-- | Unsubscribes a component from an `EventSource`. If the subscription
-- | associated with the ID has already ended this will have no effect.
unsubscribe :: forall ps o m. H.SubscriptionId -> HookM ps o m Unit
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
fork :: forall ps o m. HookM ps o m Unit -> HookM ps o m H.ForkId
fork fn = HookM $ liftF $ Fork fn identity

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall ps o m. H.ForkId -> HookM ps o m Unit
kill fid = HookM $ liftF $ Kill fid unit

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered o of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall ps o m. H.RefLabel -> HookM ps o m (Maybe DOM.Element)
getRef p = HookM $ liftF $ GetRef p identity

-- | Retrieves a `HTMLElement` value that is associated with a `Ref` in the
-- | rendered o of a component. If there is no currently rendered value (or
-- | it is not an `HTMLElement`) for the request will return `Nothing`.
getHTMLElementRef :: forall ps o m. H.RefLabel -> HookM ps o m (Maybe HTML.HTMLElement)
getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
