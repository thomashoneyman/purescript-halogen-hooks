module Halogen.Hooks.Types where

import Data.Tuple (Tuple)

-- | The kind of types used in Hooks; primitive Hooks already have this kind,
-- | and Hooks of your own should be foreign imported data types that are also
-- | types of this kind:
-- |
-- | ```purs
-- | foreign import data UseX :: Hooks.HookType
-- | ```
data HookType

-- | A unique identifier for a state produced by `useState`, which can be passed
-- | to the state functions `get`, `put`, `modify`, and `modify_` to get or
-- | modify the state.
-- |
-- | This token should NOT be modified.
-- |
-- | ```purs
-- | state /\ stateId <- useState 0
-- |
-- | let
-- |   handler = Hooks.modify_ stateId (_ + 10)
-- | ```
newtype StateId :: Type -> Type
newtype StateId state = StateId (Tuple ComponentRef Int)

-- | A unique reference for a component, which is used to track which component
-- | Hooks code is defined in to ensure that it isn't run in another component
-- | (this is unsafe, and doing so will throw an exception).
data ComponentRef

-- | The set of tokens enabling queries, child slots, and output messages when
-- | running a Hook as a component. This set of tokens is provided by the
-- | `Hooks.component` function.
-- |
-- | Hooks do not have a notion of parent / child relationships, and Halogen
-- | features like queries and outputs don't make sense in the context of Hooks.
-- | These tokens enable those features for Hooks which are being turned into
-- | components, while ensuring Hooks which are being nested are not able to
-- | access those features.
type ComponentTokens :: (Type -> Type) -> Row Type -> Type -> Type
type ComponentTokens q ps o =
  { queryToken :: QueryToken q
  , slotToken :: SlotToken ps
  , outputToken :: OutputToken o
  }

-- | A token which carries the type of queries supported by the component which
-- | is executing a Hook. Queries are specific to the parent-child component
-- | relationship, and so they are not tracked in Hook types.
-- |
-- | This token is provided by the `component` function.
data QueryToken (a :: Type -> Type)

-- | A token which carries the type of child slots supported by the component
-- | which is executing a Hook. Child slots are specific to the parent-child
-- | component relationship, and so they are not tracked in Hook types.
-- |
-- | This token is provided by the `component` function.
data SlotToken (slots :: Row Type)

-- | A token which carries the type of outputs supported by the component
-- | which is executing a Hook. Output messages slots are specific to the
-- | parent-child component relationship, and so they are not tracked in
-- | Hook types.
-- |
-- | This token is provided by the `component` function.
data OutputToken :: Type -> Type
data OutputToken output

-- | An opaque type which signifies that a set of dependencies have been captured
-- | and can be used by Hooks like `UseMemo` and `UseEffect`.
-- |
-- | This type is provided by the `captures` and `capturesWith` functions.
data MemoValues
