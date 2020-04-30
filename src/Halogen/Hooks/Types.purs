module Halogen.Hooks.Types where

-- | The set of tokens enabling queries, child slots, and output messages when
-- | running a Hook as a component. This set of tokens is provided by the
-- | `Hooks.component` function.
-- |
-- | Hooks do not have a notion of parent / child relationships, and Halogen
-- | features like queries and outputs don't make sense in the context of Hooks.
-- | These tokens enable those features for Hooks which are being turned into
-- | components, while ensuring Hooks which are being nested are not able to
-- | access those features.
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
foreign import data QueryToken :: (Type -> Type) -> Type

-- | A token which carries the type of child slots supported by the component
-- | which is executing a Hook. Child slots are specific to the parent-child
-- | component relationship, and so they are not tracked in Hook types.
-- |
-- | This token is provided by the `component` function.
foreign import data SlotToken :: # Type -> Type

-- | A token which carries the type of outputs supported by the component
-- | which is executing a Hook. Output messages slots are specific to the
-- | parent-child component relationship, and so they are not tracked in
-- | Hook types.
-- |
-- | This token is provided by the `component` function.
foreign import data OutputToken :: Type -> Type

-- | An opaque type which signifies that a set of dependencies have been captured
-- | and can be used by Hooks like `UseMemo` and `UseEffect`.
-- |
-- | This type is provided by the `captures` and `capturesWith` functions.
foreign import data MemoValues :: Type

-- | A token which carries the type of state maintained by a particular `useState`
-- | Hook. Halogen components can have only one state, but Hooks can have many;
-- | this token serves to disambiguate multiple states internally.
-- |
-- | This token is provided by the `useState` function.
newtype StateToken state = StateToken Int
