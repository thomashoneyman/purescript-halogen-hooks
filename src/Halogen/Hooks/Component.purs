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
import Halogen.Hooks.Types (QueryToken)
import Halogen.Hooks.Internal.Eval (evalHookM, interpretHook, mkEval, getState)
import Halogen.Hooks.Internal.Eval.Types (HookState(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Produces a Halogen component from a `Hook` which returns `ComponentHTML`.
-- |
-- | Components that ought to receive continuous input from a parent component
-- | should be defined as a `Hook` with one argument, the `input` type. The
-- | resulting component will re-render every time new input is received.
-- |
-- | ```purs
-- | myComponent :: forall q i o m. H.Component q i o m
-- | myComponent = Hooks.component \input -> Hooks.do
-- |   ... hook implementation
-- | ```
component
  :: forall hooks i ps o m
   . (i -> Hooked ps o m Unit hooks (H.ComponentHTML (HookM ps o m Unit) ps m))
  -> (forall q. H.Component HH.HTML q i o m)
component hookFn = componentWithQuery (\_ i -> hookFn i)

-- | Produces a Halogen component from a `Hook` which returns `ComponentHTML`,
-- | enabling the resulting component to use queries.
-- |
-- | ```purs
-- | myComponent :: forall q i o m. H.Component q i o m
-- | myComponent = Hooks.component \input queryToken -> Hooks.do
-- |   -- the query token can be used with the `useQuery hook`
-- |   Hooks.useQuery queryToken handleQuery
-- |   ... hook implementation
-- | ```
componentWithQuery
  :: forall hooks q i ps o m
   . (QueryToken q -> i -> Hooked ps o m Unit hooks (H.ComponentHTML (HookM ps o m Unit) ps m))
  -> H.Component HH.HTML q i o m
componentWithQuery inputHookFn = do
  let hookFn = inputHookFn (unsafeCoerce unit :: QueryToken q)
  H.mkComponent
    { initialState
    , render: \(HookState { result }) -> result
    , eval: mkEval evalHookM (interpretUseHookFn evalHookM) hookFn
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
