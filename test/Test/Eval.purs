-- An alternate way to evaluate hooks without components, useful for ensuring
-- the logic is correct.
module Test.Eval where

import Prelude

import Control.Monad.Free (foldFree)
import Data.Array as Array
import Data.Const (Const)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState, DriverStateX, initDriverState)
import Halogen.HTML as HH
import Halogen.Hooks (HookM, Hooked(..), UseHookF(..))
import Halogen.Hooks.Component (HookState(..), InterpretHookReason)
import Halogen.Hooks.Component as Component
import Partial.Unsafe (unsafeCrashWith)
import Test.Types (TestEvent(..), HookM')
import Unsafe.Coerce (unsafeCoerce)

type HookState' a = HookState (Const Void) LogRef () Void Aff a

type DriverResultState r a =
  DriverState HH.HTML r (HookState' a) (Const Void) (HookM' Unit) () LogRef Void

evalM
  :: forall r a
   . Ref (DriverResultState r a)
  -> H.HalogenM (HookState' a) (HookM' Unit) () Void Aff
  ~> Aff
evalM initRef = Eval.evalM mempty initRef

type LogRef = Ref (Array TestEvent)
type WithLog a = { log :: LogRef, result :: a }

writeLog :: TestEvent -> LogRef -> Unit
writeLog event ref = unsafePerformEffect do
  log <- Ref.read ref
  Ref.write (Array.snoc log event) ref

-- TODO: This is identical to `interpretUseHookFn`, but calls on `interpretHook`
interpretUseHookFn
  :: forall hooks q ps o m a
   . InterpretHookReason
  -> (LogRef -> Hooked ps o m Unit hooks a)
  -> H.HalogenM (HookState q LogRef ps o m a) (HookM ps o m Unit) ps o m Unit
interpretUseHookFn reason hookFn = do
  { input } <- Component.getState
  let Hooked (Indexed hookF) = hookFn input
  a <- foldFree (interpretHook reason hookFn) hookF
  H.modify_ (over HookState _ { result = a })

interpretHook
  :: forall hooks q ps o m a
   . InterpretHookReason
  -> (LogRef -> Hooked ps o m Unit hooks a)
  -> UseHookF ps o m
  ~> H.HalogenM (HookState q LogRef ps o m a) (HookM ps o m Unit) ps o m
interpretHook reason hookFn = case _ of
  c@(UseState initial reply) -> do
    { input: log } <- Component.getState
    let _ = writeLog ModifyState log
    Component.interpretHook reason hookFn c

  _ ->
    unsafeCrashWith "not implemented"

-- Create a new DriverState, which can be used to evaluate multiple calls to
-- evaluate test code.
initDriver :: forall r a. a -> Aff (Ref (DriverResultState r a))
initDriver initial = liftEffect do
  logRef <- Ref.new []

  stateRef <- Ref.new
    { input: logRef
    , queryFn: Nothing
    , stateCells: { queue: [], index: 0 }
    , effectCells: { queue: [], index: 0 }
    , memoCells: { queue: [], index: 0 }
    , refCells: { queue: [], index: 0 }
    , evalQueue: []
    }

  lifecycleHandlers <- Ref.new mempty

  map unDriverStateXRef do
    initDriverState
      { initialState:
          \_ ->
            HookState
              { result: HH.text ""
              , stateRef: unsafePerformEffect $ Ref.new
                  { input: logRef
                  , queryFn: Nothing
                  , stateCells: { queue: [], index: 0 }
                  , effectCells: { queue: [], index: 0 }
                  , memoCells: { queue: [], index: 0 }
                  , refCells: { queue: [], index: 0 }
                  , evalQueue: []
                  }
              }
      , render: \_ -> HH.text ""
      , eval: H.mkEval H.defaultEval
      }
      unit
      mempty
      lifecycleHandlers
  where
  unDriverStateXRef
    :: forall r' s' f' act' ps' i' o'
     . Ref (DriverStateX HH.HTML r' f' o')
    -> Ref (DriverState HH.HTML r' s' f' act' ps' i' o')
  unDriverStateXRef = unsafeCoerce

{-

-- UseHookF, enriched with the ability to insert logging where needed.
--
-- TODO: It should be possible to create a `Hook'` that works the same, except
-- you can also log things with `tell` internally. This would be really useful
-- to have in logs as output. Copy instances from `Hook`
data UseHookTellF ps o m a
  = Tell (m a)
  | UseHookF (UseHookF ps o m a)

derive instance functorUseHookTellF :: Functor m => Functor (UseHookTellF ps o m)

newtype UseHookM ps o m a = UseHookM (Free (UseHookTellF ps o m) a)

derive newtype instance functorUseHookM :: Functor (UseHookM ps o m)
derive newtype instance applyUseHookM :: Apply (UseHookM ps o m)
derive newtype instance applicativeUseHookM :: Applicative (UseHookM ps o m)
derive newtype instance bindUseHookM :: Bind (UseHookM ps o m)
derive newtype instance monadUseHookM :: Monad (UseHookM ps o m)
derive newtype instance semigroupUseHookM :: Semigroup a => Semigroup (UseHookM ps o m a)
derive newtype instance monoidUseHookM :: Monoid a => Monoid (UseHookM ps o m a)

instance monadTellUseHookM :: MonadTell w m => MonadTell w (UseHookM ps o m) where
  tell = UseHookM <<< liftF <<< Tell <<< tell

layerHook
  :: forall h ps o m
   . MonadTell (Array TestEvent) m
  => Hook ps o m h
  ~> UseHookM ps o m
layerHook (Hooked (Indexed h)) = UseHookM (substFree go h)
  where
  go :: UseHookF ps o m ~> Free (UseHookTellF ps o m)
  go = case _ of
    UseState initial reply -> do
      liftF $ Tell $ tell [ RunHooks Step ]
      liftF $ UseHookF $ UseState initial reply

    x ->
      -- TODO
      liftF $ UseHookF x

layerHookM
  :: forall ps o m
   . MonadTell (Array TestEvent) m
  => HookM ps o m
  ~> HookM ps o m
layerHookM (HookM hm) = HookM (substFree go hm)
  where
  go :: HookF ps o m ~> Free (HookF ps o m)
  go = case _ of
    Modify token f reply -> do
      -- <- getState
      -- if `f state` == `f state` the `GetState` else `ModifyState`
      liftF $ Lift $ tell [ ModifyState ]
      liftF $ Modify token f reply

    _ ->
      unsafeCrashWith "not implemented"

layerHalogenM
  :: H.HalogenM HookState' (HookM' Unit) () Void Aff
  ~> H.HalogenM HookState' (HookM' Unit) () Void (WriterT (Array TestEvent) Aff)
layerHalogenM (HalogenM hm) = HalogenM (substFree go hm)
  where
  go
    :: H.HalogenF HookState' (HookM' Unit) () Void Aff
    ~> Free (H.HalogenF HookState' (HookM' Unit) () Void (WriterT (Array TestEvent) Aff))
  go = case _ of
    H.State f -> do
      liftF $ H.Lift $ tell [ ModifyState ]
      liftF $ H.State f

    _ ->
      unsafeCrashWith "not implemented"

-}
