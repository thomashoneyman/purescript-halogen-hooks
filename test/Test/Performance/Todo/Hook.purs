module Test.Performance.Todo.Hook where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Test.Performance.Todo.Shared (CheckboxInput, CheckboxOutput(..), TodoInput, TodoOutput(..), createTodo)
import Test.Performance.Todo.Shared as Shared

_todoHook = SProxy :: SProxy "todoHook"

container :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
container = Hooks.component \_ _ -> Hooks.do
  state /\ stateId <- Hooks.useState Shared.initialContainerState

  let
    handleTodo = Just <<< case _ of
      Save t -> do
        let new = Array.findIndex (_.id >>> eq t.id) state.todos >>= \i -> Array.updateAt i t state.todos
        for_ new \todos -> Hooks.modify_ stateId _ { todos = todos }

      SetCompleted id complete -> do
        if complete then
          Hooks.modify_ stateId _ { completed = Set.insert id state.completed }
        else
          Hooks.modify_ stateId _ { completed = Set.delete id state.completed }

  Hooks.useLifecycleEffect do
    filled <- liftEffect $ Shared.fillContainerState state
    Hooks.put stateId filled
    pure Nothing

  Hooks.pure do
    let
      todos = state.todos <#> \t ->
        HH.slot Shared._todo t.id todo { todo: t, completed: state.completed } handleTodo

    HH.div_
      [ HH.button
          [ HP.id_ Shared.addNewId
          , HE.onClick \_ -> Just do
              newState <- liftEffect $ createTodo state
              Hooks.put stateId newState
          ]
          [ HH.text "Add New" ]
      , HH.div
          [ HP.id_ Shared.todosId ]
          todos
      ]

todo :: forall q m. MonadAff m => H.Component HH.HTML q TodoInput TodoOutput m
todo = Hooks.memoComponent (eq `on` _.todo.id && eq `on` _.completed) \{ outputToken } input -> Hooks.do
  description /\ descriptionId <- Hooks.useState input.todo.description

  let
    handleCheckbox (Check bool) = Just do
      Hooks.raise outputToken $ SetCompleted input.todo.id bool

  Hooks.pure do
    HH.div_
      [ HH.input
          [ HP.id_ (Shared.editId input.todo.id)
          , HE.onValueInput (Just <<< Hooks.put descriptionId)
          , HP.value description
          ]
      , HH.slot Shared._checkbox unit checkbox { id: input.todo.id, completed: input.completed } handleCheckbox
      , HH.button
          [ HP.id_ (Shared.saveId input.todo.id)
          , HE.onClick \_ -> Just do
              Hooks.raise outputToken $ Save { id: input.todo.id, description }
          ]
          [ HH.text "Save Changes" ]
      ]

checkbox :: forall q m. MonadAff m => H.Component HH.HTML q CheckboxInput CheckboxOutput m
checkbox = Hooks.component \{ outputToken } input -> Hooks.do
  Hooks.pure do
    HH.input
      [ HP.id_ (Shared.checkId input.id)
      , HP.checked $ Set.member input.id input.completed
      , HP.type_ HP.InputCheckbox
      , HE.onChecked \checked -> Just do
          Hooks.raise outputToken $ Check checked
      ]
