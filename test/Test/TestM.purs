module Test.TestM where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Halogen.Hooks.Component (HookState)

-- While a little tedious, this newtype is necessary in order to provide a
-- type-variable-free `MonadState` instance for `TestM`.
newtype HookState' = HookState' (HookState (Const Void) Unit () Void Aff)

derive instance newtypeHookState' :: Newtype HookState' _

-- | TestF is a reinterpretation of the HalogenF component algebra as an alternate
-- | target for Hooks tests
data TestF a
  = State (HookState' -> Tuple a HookState')

instance functorTestF :: Functor TestF where
  map f = case _ of
    State k -> State (lmap f <<< k)

newtype TestM a = TestM (Free TestF a)

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance semigroupTestM :: Semigroup a => Semigroup (TestM a)
derive newtype instance monoidTestM :: Monoid a => Monoid (TestM a)

instance monadRecTestM :: MonadRec TestM where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance monadStateTestM :: MonadState HookState' TestM where
  state = TestM <<< liftF <<< State
