module Infrastructure.InMemoryTaskRepository
  ( newInMemoryTaskRepository
  )
where

import Application.Port.TaskRepository (TaskRepository (..))
import Control.Concurrent.MVar (modifyMVar, newMVar, readMVar)
import Data.List (find, partition)
import Domain.Task
  ( Outcome (..)
  , OutcomeInput (..)
  , TaskError (..)
  , TaskItem
  , assembleOutcome
  , createTask
  , taskId
  , updateTask
  )

data Store = Store [TaskItem] [Outcome]

newInMemoryTaskRepository :: [TaskItem] -> IO (TaskRepository IO)
newInMemoryTaskRepository initialTasks = do
  store <- newMVar (Store initialTasks [])
  pure
    TaskRepository
      { listTasks = do
          Store tasks _ <- readMVar store
          pure tasks
      , findTask = \identifier -> do
          Store tasks _ <- readMVar store
          pure (find ((== identifier) . taskId) tasks)
      , createStoredTask = \input ->
          modifyMVar store $ \(Store tasks outcomes) ->
            let task = createTask (nextId tasks) input
             in pure (Store (tasks <> [task]) outcomes, task)
      , updateStoredTask = \identifier input ->
          modifyMVar store $ \(Store tasks outcomes) ->
            case find ((== identifier) . taskId) tasks of
              Nothing -> pure (Store tasks outcomes, Nothing)
              Just existing ->
                let updated = updateTask existing input
                 in pure (Store (map (replace updated) tasks) outcomes, Just updated)
      , replaceStoredTask = \updated ->
          modifyMVar store $ \(Store tasks outcomes) ->
            pure (Store (map (replace updated) tasks) outcomes, ())
      , deleteStoredTask = \identifier ->
          modifyMVar store $ \(Store tasks outcomes) ->
            let (deleted, remaining) = partition ((== identifier) . taskId) tasks
             in pure (Store remaining outcomes, not (null deleted))
      , listOutcomes = do
          Store _ outcomes <- readMVar store
          pure outcomes
      , createStoredOutcome = \input ->
          modifyMVar store $ \(Store storedTasks outcomes) ->
            let selectedTasks =
                  filter (\task -> taskId task `elem` inputSourceTaskIds input) storedTasks
             in if length selectedTasks /= length (inputSourceTaskIds input)
                  then pure (Store storedTasks outcomes, Left TaskNotApproved)
                  else case assembleOutcome (nextOutcomeId outcomes) input selectedTasks of
                    Left err -> pure (Store storedTasks outcomes, Left err)
                    Right outcome ->
                      pure
                        ( Store storedTasks (outcomes <> [outcome])
                        , Right outcome
                        )
      }

nextId :: [TaskItem] -> Int
nextId [] = 1
nextId tasks = maximum (map taskId tasks) + 1

replace :: TaskItem -> TaskItem -> TaskItem
replace updated task
  | taskId updated == taskId task = updated
  | otherwise = task

nextOutcomeId :: [Outcome] -> Int
nextOutcomeId [] = 1
nextOutcomeId outcomes = maximum (map outcomeId outcomes) + 1
