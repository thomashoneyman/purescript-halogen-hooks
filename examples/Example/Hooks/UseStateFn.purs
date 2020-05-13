module Example.Hooks.UseStateFn (useStateFn) where

import Prelude

import Data.Functor.Indexed (imap)
import Data.Tuple.Nested (type (/\))
import Halogen.Hooks (Hook, StateId, UseState)
import Halogen.Hooks as Hooks

useStateFn
  :: forall state m a
   . (StateId state -> a)
  -> state
  -> Hook m (UseState state) (state /\ a)
useStateFn fn initial = imap (map fn) (Hooks.useState initial)
