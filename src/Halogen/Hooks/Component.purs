module Halogen.Hooks.Component where

import Prelude

import Control.Monad.Free (foldFree)
import Control.Monad.ST.Class (liftST)
import Data.Array.ST as Array.ST
import Data.Indexed (Indexed(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks.Hook (Hooked(..))
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.Internal.Eval (evalHookM, interpretHook, mkEval)
import Halogen.Hooks.Internal.Eval.Types (State(..), toHalogenM)
import Halogen.Hooks.Internal.Eval.Types as ET
import Halogen.Hooks.Types (ComponentTokens, OutputToken, QueryToken, SlotToken)
import Record.ST as Record.ST
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
  :: forall hooks q i ps o m
   . (i -> i -> Boolean)
  -> (ComponentTokens q ps o -> i -> Hooked m Unit hooks (H.ComponentHTML (HookM m Unit) ps m))
  -> H.Component HH.HTML q i o m
memoComponent eqInput inputHookFn = do
  let
    queryToken = unsafeCoerce unit :: QueryToken q
    slotToken = unsafeCoerce unit :: SlotToken ps
    outputToken = unsafeCoerce unit :: OutputToken o
    hookFn = inputHookFn { queryToken, slotToken, outputToken }

  H.mkComponent
    { initialState
    , render: \(State { result }) -> result
    , eval: toHalogenM slotToken outputToken <<< mkEval eqInput evalHookM (interpretUseHookFn evalHookM) hookFn
    }
  where
  -- WARNING: If you update this function, make sure to apply the same update
  -- to the tests, which use their own version of this function. The test function
  -- should be identical, except with the addition of logging.
  interpretUseHookFn runHookM reason hookFn = do
    input <- ET.getInternalField ET._input
    let Hooked (Indexed hookF) = hookFn input
    a <- foldFree (interpretHook runHookM (\r -> interpretUseHookFn runHookM r hookFn) reason hookFn) hookF
    H.modify_ (over State _ { result = a })
    pure a

  initialState input =
    State
      { result: HH.text ""
      , internal: unsafePerformEffect $ liftST $ Record.ST.thaw
          { input
          , queryFn: Nothing
          , evalQueue: unsafePerformEffect $ liftST Array.ST.empty
          , stateDirty: false
          , stateCells: unsafePerformEffect $ liftST Array.ST.empty
          , stateIndex: 0
          , effectCells: unsafePerformEffect $ liftST Array.ST.empty
          , effectIndex: 0
          , memoCells: unsafePerformEffect $ liftST Array.ST.empty
          , memoIndex: 0
          , refCells: unsafePerformEffect $ liftST Array.ST.empty
          , refIndex: 0
          }
      }
