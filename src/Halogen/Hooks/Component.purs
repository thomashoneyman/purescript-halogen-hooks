module Halogen.Hooks.Component where

import Prelude

import Control.Monad.Free (foldFree)
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
import Halogen.Hooks.Types (ComponentTokens, OutputToken, QueryToken, SlotToken)
import Unsafe.Coerce (unsafeCoerce)

-- | Produces a Halogen component from a `Hook` which returns `ComponentHTML`.
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
component
  :: forall hooks q i ps o m
   . (ComponentTokens q ps o -> i -> Hooked m Unit hooks (H.ComponentHTML (HookM m Unit) ps m))
  -> H.Component HH.HTML q i o m
component inputHookFn = do
  let
    queryToken = unsafeCoerce unit :: QueryToken q
    slotToken = unsafeCoerce unit :: SlotToken ps
    outputToken = unsafeCoerce unit :: OutputToken o
    hookFn = inputHookFn { queryToken, slotToken, outputToken }

  H.mkComponent
    { initialState
    , render: \(HookState { result }) -> result
    , eval: toHalogenM slotToken outputToken <<< mkEval evalHookM (interpretUseHookFn evalHookM) hookFn
    }
  where
  -- WARNING: If you update this function, make sure to apply the same update
  -- to the tests, which use their own version of this function. The test function
  -- should be identical, except with the addition of logging.
  interpretUseHookFn runHookM reason hookFn = do
    { input } <- getState
    let Hooked (Indexed hookF) = hookFn input
    a <- foldFree (interpretHook runHookM (\r -> interpretUseHookFn runHookM r hookFn) reason hookFn) hookF
    H.modify_ (over HookState _ { result = a })
    pure a

  initialState input =
    HookState
      { result: HH.text ""
      , stateRef: unsafePerformEffect $ Ref.new
          { input
          , queryFn: Nothing
          , stateCells: { queue: [], index: 0 }
          , effectCells: { queue: [], index: 0 }
          , memoCells: { queue: [], index: 0 }
          , refCells: { queue: [], index: 0 }
          , evalQueue: []
          }
      }
