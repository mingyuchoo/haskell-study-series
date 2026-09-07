module Application.Port.TaskRepository
  ( TaskRepository (..)
  )
where

import Domain.Task (Outcome, OutcomeInput, TaskInput, TaskItem)

data TaskRepository m = TaskRepository
  { listTasks :: m [TaskItem]
  , findTask :: Int -> m (Maybe TaskItem)
  , createStoredTask :: TaskInput -> m TaskItem
  , updateStoredTask :: Int -> TaskInput -> m (Maybe TaskItem)
  , replaceStoredTask :: TaskItem -> m ()
  , deleteStoredTask :: Int -> m Bool
  , listOutcomes :: m [Outcome]
  , createStoredOutcome :: OutcomeInput -> [TaskItem] -> m Outcome
  }
