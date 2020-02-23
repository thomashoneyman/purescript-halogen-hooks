module Halogen.EvalHookM where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Array as Array
import Data.Maybe (fromJust)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | The EvalHook API: a set of primitive building blocks that can be used as
-- | an alternate interface to HalogenM when evaluating hooks. Implemented so
-- | that multiple states can be accessed by different hooks.
data EvalHookF (o :: Type) (m :: Type -> Type) a
  = Modify (StateToken StateValue) (StateValue -> StateValue) (StateValue -> a)
  | Lift (m a)
  | Raise o a

derive instance functorHookF :: Functor m => Functor (EvalHookF out m)

-- | The Hook effect monad, an interface to the HalogenM component eval effect monad
newtype EvalHookM o m a = EvalHookM (Free (EvalHookF o m) a)

derive newtype instance functorEvalHookM :: Functor (EvalHookM o m)
derive newtype instance applyEvalHookM :: Apply (EvalHookM o m)
derive newtype instance applicativeEvalHookM :: Applicative (EvalHookM o m)
derive newtype instance bindEvalHookM :: Bind (EvalHookM o m)
derive newtype instance monadEvalHookM :: Monad (EvalHookM o m)
derive newtype instance semigroupEvalHookM :: Semigroup a => Semigroup (EvalHookM o m a)
derive newtype instance monoidEvalHookM :: Monoid a => Monoid (EvalHookM o m a)

instance monadEffectEvalHookM :: MonadEffect m => MonadEffect (EvalHookM output m) where
  liftEffect = EvalHookM <<< liftF <<< Lift <<< liftEffect

instance monadAffEvalHookM :: MonadAff m => MonadAff (EvalHookM output m) where
  liftAff = EvalHookM <<< liftF <<< Lift <<< liftAff

--  State

foreign import data StateValue :: Type

toStateValue :: forall state. state -> StateValue
toStateValue = unsafeCoerce

fromStateValue :: forall state. StateValue -> state
fromStateValue = unsafeCoerce

-- Used to uniquely identify a cell in state as well as its type so it can be
-- modified safely by users but is also available in a heterogeneous collection
-- in component state. Should not have its constructor exported.
newtype StateToken state = StateToken StateId

get :: forall state out m. StateToken state -> EvalHookM out m state
get token = modify token identity

put :: forall state out m. StateToken state -> state -> EvalHookM out m Unit
put token state = modify_ token (const state)

modify_ :: forall state out m. StateToken state -> (state -> state) -> EvalHookM out m Unit
modify_ token = map (const unit) <<< modify token

modify :: forall state out m. StateToken state -> (state -> state) -> EvalHookM out m state
modify token f = EvalHookM $ liftF $ Modify token' f' state
  where
  token' :: StateToken StateValue
  token' = unsafeCoerce token

  f' :: StateValue -> StateValue
  f' = toStateValue <<< f <<< fromStateValue

  state :: StateValue -> state
  state = fromStateValue

-- Outputs

raise :: forall out m. out -> EvalHookM out m Unit
raise output = EvalHookM $ liftF $ Raise output unit

-- Interpreter

foreign import data SlotValue :: # Type

hideSlotsHalogenM
  :: forall state action slots out m
   . H.HalogenM state action slots out m
  ~> H.HalogenM state action SlotValue out m
hideSlotsHalogenM m = unsafeCoerce m

hideSlotsComponentHTML
  :: forall action slots m
   . H.ComponentHTML action slots m
  -> H.ComponentHTML action SlotValue m
hideSlotsComponentHTML m = unsafeCoerce m

type HookState i o m =
  { state :: QueueState
  , html :: H.ComponentHTML (EvalHookM o m Unit) SlotValue m
  , input :: i
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

interpretEvalHook :: forall i a ps o m. EvalHookF o m ~> H.HalogenM (HookState i o m) a ps o m
interpretEvalHook = case _ of
  Modify (StateToken token) f reply -> do
    state <- H.get

    let
      v = unsafeGetState token state.state.queue
      v' = f v

    H.put $ state { state { queue = unsafeSetState token v' state.state.queue } }
    pure (reply v')

  Raise o a -> do
    H.raise o
    pure a

  Lift f -> H.HalogenM $ liftF $ H.Lift f

-- Utilities for updating state

unsafeGetState :: StateId -> Array StateValue -> StateValue
unsafeGetState (StateId index) array = unsafePartial (Array.unsafeIndex array index)

unsafeSetState :: StateId -> StateValue -> Array StateValue -> Array StateValue
unsafeSetState (StateId index) a array = unsafePartial (fromJust (Array.modifyAt index (const a) array))
