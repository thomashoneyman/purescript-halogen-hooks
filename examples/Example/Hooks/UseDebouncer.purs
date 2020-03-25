module Example.Hooks.UseDebouncer
  ( useDebouncer
  , UseDebouncer
  )
  where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (HookM, Hook, UseRef)
import Halogen.Hooks as Hooks

newtype UseDebouncer a hooks =
  UseDebouncer (UseRef (Maybe a) (UseRef (Maybe Debouncer) hooks))

derive instance newtypeUseDebouncer :: Newtype (UseDebouncer a hooks) _

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

useDebouncer
  :: forall slots output m a
   . MonadAff m
  => Milliseconds
  -> (a -> HookM slots output m Unit)
  -> Hook slots output m (UseDebouncer a) (a -> HookM slots output m Unit)
useDebouncer ms fn = Hooks.wrap Hooks.do
  _ /\ debounceRef <- Hooks.useRef Nothing
  _ /\ valRef <- Hooks.useRef Nothing

  let
    debounceFn x = do
      debouncer <- liftEffect do
        Ref.write (Just x) valRef
        Ref.read debounceRef

      case debouncer of
        Nothing -> do
          var <- liftAff AVar.empty
          fiber <- liftAff $ forkAff do
            delay ms
            AVar.put unit var

          _ <- Hooks.fork do
            _ <- liftAff $ AVar.take var
            val <- liftEffect do
              Ref.write Nothing debounceRef
              Ref.read valRef
            traverse_ fn val

          liftEffect do
            Ref.write (Just { var, fiber }) debounceRef

        Just db -> do
          let var = db.var
          fiber <- liftAff do
            killFiber (error "Time's up!") db.fiber
            forkAff do
              delay ms
              AVar.put unit var

          liftEffect $ Ref.write (Just { var, fiber }) debounceRef

  Hooks.pure debounceFn
