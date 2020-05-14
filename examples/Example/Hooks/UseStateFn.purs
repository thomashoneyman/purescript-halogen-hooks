module Example.Hooks.UseStateFn (useStateFn) where

import Prelude

import Data.Tuple.Nested (type (/\))
import Halogen.Hooks (Hook, StateId, UseState)
import Halogen.Hooks as Hooks

useStateFn
  :: forall state m a
   . (StateId state -> a)
  -> state
  -> Hook m (UseState state) (state /\ a)
useStateFn fn initial = map (map fn) (Hooks.useState initial)
