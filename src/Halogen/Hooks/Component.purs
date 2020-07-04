module Halogen.Hooks.Component where

import Prelude

import Control.Monad.Free (substFree)
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks.Hook (Hooked(..))
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.Internal.Eval (evalHookM, interpretHook, mkEval, getState)
import Halogen.Hooks.Internal.Eval.Types (HookState(..), toHalogenM)
import Halogen.Hooks.Types (ComponentRef, ComponentTokens, OutputToken, QueryToken, SlotToken)
import Unsafe.Coerce (unsafeCoerce)

-- | Produces a Halogen component from a `Hook` which returns `ComponentHTML`.
-- | If you need to control whether Hooks evaluate when new input is received,
-- | see `memoComponent`.
-- |
-- | Tokens are provided which enable access to component-only features like
-- | queries, output messages, and child slots, which don't make sense in a pure
-- | Hook context.
-- |
-- | ```purs
-- | myComponent :: forall q i o m. H.Component q i o m
-- | myComponent = Hooks.component \tokens input -> Hooks.do
-- |   ... hook implementation
-- | ```
-- |
-- | If you don't need to use tokens or input, you can use underscores to throw
-- | away those arguments.
-- |
-- | ```purs
-- | myComponent :: forall q i o m. H.Component q i o m
-- | myComponent = Hooks.component \_ _ -> Hooks.do
-- |   ... hook implementation
-- | ```
-- |
-- | If you are using tokens provided by the `component` function, you will have
-- | better type inference if you annotate the token type:
-- |
-- | ```purs
-- | type Tokens = ComponentTokens MyQuery MySlots MyOutput
-- |
-- | myComponent :: forall i m. H.Component MyQuery i MyOutput m
-- | myComponent = Hooks.component \(tokens :: Tokens) _ -> Hooks.do
-- |   ... hook implementation
-- |
-- | Use type variables to substitue unused token types:
-- |
-- | ```purs
-- | type Tokens s o = ComponentTokens MyQuery s o
-- |
-- | myComponent :: forall i o m. H.Component MyQuery i o m
-- | myComponent = Hooks.component \(tokens :: Tokens _ o) _ -> Hooks.do
-- |   ... hook implementation
-- | ```
component
  :: forall hooks q i ps o m
   . (ComponentTokens q ps o -> i -> Hooked m Unit hooks (H.ComponentHTML (HookM m Unit) ps m))
  -> H.Component HH.HTML q i o m
component = memoComponent (\_ _ -> false)

-- | A version of `component` which allows you to decide whether or not to send
-- | new input to the hook function based on an equality predicate. Halogen
-- | components send input to children on each render, which can cause a
-- | performance issue in some cases.
-- |
-- | ```purs
-- | myComponent :: forall q o m. H.Component q Int o m
-- | myComponent = Hooks.memoComponent eq \tokens input -> Hooks.do
-- |   -- This hook implementation will not run when it receives new input
-- |   -- unless the `Int` has changed.
-- | ```
-- |
-- | Some input data may be more expensive to compute equality for than to simply
-- | send input again. In these cases you may want to write a more sophisticated
-- | equality function -- for example, only checking by a unique ID.
-- |
-- | ```purs
-- | type User = { uuid :: Int, info :: HugeObject }
-- |
-- | eqUser :: User -> User -> Boolean
-- | eqUser userA userB = userA.uuid == userB.uuid
-- |
-- | myComponent :: forall q o m. H.Component q User o m
-- | myComponent = Hooks.memoComponent eqUser \_ input -> Hooks.do
-- |   -- This hook implementation will not run when it receives new input
-- |   -- unless the `User`'s id has changed.
-- | ```
memoComponent
  :: forall hooks q i s o m
   . (i -> i -> Boolean)
  -> (ComponentTokens q s o -> i -> Hooked m Unit hooks (H.ComponentHTML (HookM m Unit) s m))
  -> H.Component HH.HTML q i o m
memoComponent eqInput inputHookFn = do
  let
    queryToken = unsafeCoerce {} :: QueryToken q
    slotToken = unsafeCoerce {} :: SlotToken s
    outputToken = unsafeCoerce {} :: OutputToken o
    hookFn = inputHookFn { queryToken, slotToken, outputToken }

  H.mkComponent
    { initialState
    , render: \(HookState { result }) -> result
    , eval: toHalogenM slotToken outputToken <<< mkEval eqInput evalHookM (interpretUseHookFn evalHookM) hookFn
    }
  where
  -- WARNING: If you update this function, make sure to apply the same update
  -- to the tests, which use their own version of this function. The test function
  -- should be identical, except with the addition of logging.
  interpretUseHookFn runHookM reason hookFn = do
    { input } <- H.HalogenM getState
    let Hooked (Indexed hookF) = hookFn input
    a <- H.HalogenM $ substFree (interpretHook runHookM (\r -> interpretUseHookFn runHookM r hookFn) reason hookFn) hookF
    H.modify_ (over HookState _ { result = a })
    pure a

  initialState input =
    HookState
      { result: HH.text ""
      , stateRef: unsafePerformEffect $ Ref.new
          { input
          , componentRef: unsafeCoerce {} :: ComponentRef
          , queryFn: Nothing
          , stateCells: { queue: [], index: 0 }
          , effectCells: { queue: [], index: 0 }
          , memoCells: { queue: [], index: 0 }
          , refCells: { queue: [], index: 0 }
          , evalQueue: []
          , stateDirty: false
          }
      }
