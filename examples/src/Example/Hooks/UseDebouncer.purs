module Example.Hooks.UseDebouncer
  ( useDebouncer
  , UseDebouncer
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, UseRef)
import Halogen.Hooks as Hooks

foreign import data UseDebouncer :: Type -> Hooks.HookType

type UseDebouncer' a = UseRef (Maybe Debouncer) <> UseRef (Maybe a) <> Hooks.Pure

instance HookNewtype (UseDebouncer a) (UseDebouncer' a)

type Debouncer = { var :: AVar Unit, fiber :: Fiber Unit }

useDebouncer
  :: forall m a
   . MonadAff m
  => Milliseconds
  -> (a -> HookM m Unit)
  -> Hook m (UseDebouncer a) (a -> HookM m Unit)
useDebouncer ms fn = Hooks.wrap hook
  where
  hook :: Hook m (UseDebouncer' a) (a -> HookM m Unit)
  hook = Hooks.do
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
