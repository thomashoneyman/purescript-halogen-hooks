-- | A replacement for `Halogen.Query.HalogenM` which supports a near-identical
-- | API, but adjusted for compatibility with hooks. All functions available in
-- | `HalogenM` are still available here, but some have modified behavior (for
-- | example, the state functions `get`, `put`, and `modify` take a state
-- | identifier as their first argument).
module Halogen.Hooks.HookM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader (class MonadAsk, class MonadTrans, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell)
import Control.Monad.Writer as MR
import Control.Parallel (class Parallel)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Request, Tell, mkRequest, mkTell)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Hooks.Internal.Types (OutputValue, SlotType, StateValue, fromStateValue, toOutputValue, toStateValue)
import Halogen.Hooks.Types (OutputToken, SlotToken, StateId)
import Halogen.Query.ChildQuery as CQ
import Halogen.Subscription as HS
import Prim.Row as Row
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM
import Web.HTML as HTML
import Web.HTML.HTMLElement as HTMLElement

-- | A DSL compatible with HalogenM which is used to write effectful code
-- | for Hooks.
data HookF m a
  = Modify (StateId StateValue) (StateValue -> StateValue) (StateValue -> a)
  | Subscribe (H.SubscriptionId -> HS.Emitter (HookM m Unit)) (H.SubscriptionId -> a)
  | Unsubscribe H.SubscriptionId a
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox SlotType a)
  | Raise OutputValue a
  | Par (HookAp m a)
  | Fork (HookM m Unit) (H.ForkId -> a)
  | Kill H.ForkId a
  | GetRef H.RefLabel (Maybe DOM.Element -> a)

derive instance Functor m => Functor (HookF m)

-- | The Hook effect monad, used to write effectful code in Hooks functions.
-- | This monad is fully compatible with `HalogenM`, meaning all functionality
-- | available for `HalogenM` is available in `HookM`.
newtype HookM m a = HookM (Free (HookF m) a)

derive newtype instance Functor (HookM m)
derive newtype instance Apply (HookM m)
derive newtype instance Applicative (HookM m)
derive newtype instance Bind (HookM m)
derive newtype instance Monad (HookM m)
derive newtype instance Semigroup a => Semigroup (HookM m a)
derive newtype instance Monoid a => Monoid (HookM m a)

instance MonadEffect m => MonadEffect (HookM m) where
  liftEffect = HookM <<< liftF <<< Lift <<< liftEffect

instance MonadAff m => MonadAff (HookM m) where
  liftAff = HookM <<< liftF <<< Lift <<< liftAff

instance MonadTrans HookM where
  lift = HookM <<< liftF <<< Lift

instance MonadRec (HookM m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance MonadAsk r m => MonadAsk r (HookM m) where
  ask = HookM $ liftF $ Lift ask

instance MonadTell w m => MonadTell w (HookM m) where
  tell = HookM <<< liftF <<< Lift <<< MR.tell

instance MonadThrow e m => MonadThrow e (HookM m) where
  throwError = HookM <<< liftF <<< Lift <<< throwError

-- | An applicative-only version of `HookM` to allow for parallel evaluation.
newtype HookAp m a = HookAp (FreeAp (HookM m) a)

derive instance Newtype (HookAp m a) _
derive newtype instance Functor (HookAp m)
derive newtype instance Apply (HookAp m)
derive newtype instance Applicative (HookAp m)

instance Parallel (HookAp m) (HookM m) where
  parallel = HookAp <<< liftFreeAp
  sequential = HookM <<< liftF <<< Par

-- | Get a piece of state using an identifier received from the `useState` hook.
-- |
-- | ```purs
-- | _ /\ countId :: StateId Int <- Hooks.useState 0
-- |
-- | let
-- |   onClick = do
-- |     count :: Int <- Hooks.get countId
-- |     ...
-- | ```
get :: forall state m. StateId state -> HookM m state
get identifier = modify identifier identity

-- | Modify a piece of state using an identifier received from the `useState` hook.
-- |
-- | ```purs
-- | _ /\ countId :: StateId Int <- Hooks.useState 0
-- |
-- | let
-- |   onClick = do
-- |     Hooks.modify_ countId (_ + 10)
-- | ```
modify_ :: forall state m. StateId state -> (state -> state) -> HookM m Unit
modify_ identifier = map (const unit) <<< modify identifier

-- | Modify a piece of state using an identifier received from the `useState` hook,
-- | returning the new state.
-- |
-- | ```purs
-- | _ /\ countId :: StateId Int <- Hooks.useState 0
-- |
-- | let
-- |   onClick = do
-- |     count :: Int <- Hooks.modify countId (_ + 10)
-- |     ...
-- | ```
modify :: forall state m. StateId state -> (state -> state) -> HookM m state
modify identifier f = HookM $ liftF $ Modify identifier' f' state
  where
  identifier' :: StateId StateValue
  identifier' = unsafeCoerce identifier

  f' :: StateValue -> StateValue
  f' = toStateValue <<< f <<< fromStateValue

  state :: StateValue -> state
  state = fromStateValue

-- | Overwrite a piece of state using an identifier received from the `useState` hook.
-- |
-- | ```purs
-- | _ /\ countId :: StateId Int <- Hooks.useState 0
-- |
-- | let
-- |   onClick = do
-- |     Hooks.put countId 10
-- | ```
put :: forall state m. StateId state -> state -> HookM m Unit
put identifier state = modify_ identifier (const state)

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
  -> Proxy label
  -> slot
  -> query a
  -> HookM m (Maybe a)
query _ label p q =
  HookM $ liftF $ ChildQuery $ box $ CQ.mkChildQueryBox do
    CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup label p) q identity
  where
  box :: CQ.ChildQueryBox ps ~> CQ.ChildQueryBox SlotType
  box = unsafeCoerce

-- | Send a query-request to a child of a component at the specified slot. Requires a
-- | token carrying the slot type of the component, which is provided by the
-- | `Hooks.component` function.
request
  :: forall m label ps query o' slot a _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SlotToken ps
  -> Proxy label
  -> slot
  -> Request query a
  -> HookM m (Maybe a)
request slotToken label slot req = query slotToken label slot $ mkRequest req

-- | Send a tell-request to a child of a component at the specified slot. Requires a
-- | token carrying the slot type of the component, which is provided by the
-- | `Hooks.component` function.
tell
  :: forall m label ps query o' slot _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SlotToken ps
  -> Proxy label
  -> slot
  -> Tell query
  -> HookM m Unit
tell slotToken label slot req = void $ query slotToken label slot $ mkTell req

-- | Send a query to all children of a component at the specified slot. Requires
-- | a token carrying the slot type of the component, which is provided by the
-- | `Hooks.component` function.
queryAll
  :: forall m label ps query o' slot a _1
   . Row.Cons label (H.Slot query o' slot) _1 ps
  => IsSymbol label
  => Ord slot
  => SlotToken ps
  -> Proxy label
  -> query a
  -> HookM m (Map slot a)
queryAll _ label q =
  HookM $ liftF $ ChildQuery $ box $ CQ.mkChildQueryBox do
    CQ.ChildQuery (\k -> map catMapMaybes <<< traverse k <<< Slot.slots label) q identity
  where
  box :: CQ.ChildQueryBox ps ~> CQ.ChildQueryBox SlotType
  box = unsafeCoerce

  catMapMaybes :: forall k v. Ord k => Map k (Maybe v) -> Map k v
  catMapMaybes = foldrWithIndex (\k v acc -> maybe acc (flip (Map.insert k) acc) v) Map.empty

-- | Subscribes a component to an `Emitter`. When a component is disposed of
-- | any active subscriptions will automatically be stopped and no further subscriptions
-- | will be possible during finalization.
subscribe :: forall m. HS.Emitter (HookM m Unit) -> HookM m H.SubscriptionId
subscribe es = HookM $ liftF $ Subscribe (\_ -> es) identity

-- | An alternative to `subscribe`, intended for subscriptions that unsubscribe
-- | themselves. Instead of returning the `SubscriptionId` from `subscribe'`, it
-- | is passed into an `Emitter` constructor. This allows emitted queries
-- | to include the `SubscriptionId`, rather than storing it in the state of the
-- | component.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe' :: forall m. (H.SubscriptionId -> HS.Emitter (HookM m Unit)) -> HookM m Unit
subscribe' esc = HookM $ liftF $ Subscribe esc (const unit)

-- | Unsubscribes a component from an `Emitter`. If the subscription
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
