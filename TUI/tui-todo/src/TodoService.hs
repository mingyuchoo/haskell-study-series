{-# LANGUAGE FlexibleContexts #-}

-- | Todo Business Logic (Pure with Tagless Final)
--
-- This module contains business logic that depends only on effect algebras.
-- All functions are polymorphic over the monad, enabling easy testing.
module TodoService
    ( createNewTodo
    , cycleStatusForward
    , deleteTodoById
    , findTodoById
    , loadAllTodos
    , updateTodoById
    ) where

import qualified DB

import           Data.List (find)

import           Effects

-- | Load all todos from repository
loadAllTodos :: MonadTodoRepo m => m [DB.TodoRow]
loadAllTodos = getAllTodos

-- | Create a new todo with all fields
createNewTodo :: MonadTodoRepo m
              => String
              -> Maybe String
              -> Maybe String
              -> Maybe String
              -> m DB.TodoId
createNewTodo = createTodoWithFields

-- | Update an existing todo
updateTodoById :: MonadTodoRepo m
               => DB.TodoId
               -> String
               -> Maybe String
               -> Maybe String
               -> Maybe String
               -> m ()
updateTodoById = updateTodoFields

-- | Delete a todo by ID
deleteTodoById :: MonadTodoRepo m => DB.TodoId -> m ()
deleteTodoById = deleteTodo

-- | Find a todo by ID from a list
findTodoById :: DB.TodoId -> [DB.TodoRow] -> Maybe DB.TodoRow
findTodoById tid = find (\row -> DB.todoId row == tid)

-- | Cycle todo status forward: Registered -> InProgress -> Cancelled -> Completed -> Registered
cycleStatusForward :: MonadTodoRepo m => DB.TodoId -> String -> m ()
cycleStatusForward tid currentStatus =
    case currentStatus of
        "registered"  -> transitionToInProgress tid
        "in_progress" -> transitionToCancelled tid
        "cancelled"   -> transitionToCompleted tid
        "completed"   -> transitionToRegistered tid
        _             -> pure ()
