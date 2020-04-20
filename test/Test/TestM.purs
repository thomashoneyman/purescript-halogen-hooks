module Test.TestM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

-- | TestF is a reinterpretation of the HalogenF component algebra as an alternate
-- | target for Hooks tests
data TestF state m a
  = State (state -> Tuple a state)
  | Lift (m a)

instance functorTestF :: Functor m => Functor (TestF state m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Lift q -> Lift (map f q)

newtype TestM state m a = TestM (Free (TestF state m) a)

derive newtype instance functorTestM :: Functor (TestM state m)
derive newtype instance applyTestM :: Apply (TestM state m)
derive newtype instance applicativeTestM :: Applicative (TestM state m)
derive newtype instance bindTestM :: Bind (TestM state m)
derive newtype instance monadTestM :: Monad (TestM state m)
derive newtype instance semigroupTestM :: Semigroup a => Semigroup (TestM state m a)
derive newtype instance monoidTestM :: Monoid a => Monoid (TestM state m a)

instance monadEffectTestM :: MonadEffect m => MonadEffect (TestM state m) where
  liftEffect = TestM <<< liftF <<< Lift <<< liftEffect

instance monadAffTestM :: MonadAff m => MonadAff (TestM state m) where
  liftAff = TestM <<< liftF <<< Lift <<< liftAff

instance monadTransTestM :: MonadTrans (TestM state) where
  lift = TestM <<< liftF <<< Lift

instance monadRecTestM :: MonadRec (TestM state m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance monadStateTestM :: MonadState state (TestM state m) where
  state = TestM <<< liftF <<< State

instance monadAskTestM :: MonadAsk r m => MonadAsk r (TestM state m) where
  ask = TestM $ liftF $ Lift ask

instance monadTellTestM :: MonadTell w m => MonadTell w (TestM state m) where
  tell = TestM <<< liftF <<< Lift <<< tell

instance monadThrowTestM :: MonadThrow e m => MonadThrow e (TestM state m) where
  throwError = TestM <<< liftF <<< Lift <<< throwError
