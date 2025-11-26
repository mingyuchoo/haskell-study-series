{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | MTL-based application logic (Effectful)
--
-- This module contains effectful operations for managing todos.
-- All functions perform IO operations through the AppM monad.
--
-- Effects:
--   - Database operations (via DB module)
--   - IO operations (via MonadIO)
--
-- Purity: NONE - All exported functions are effectful
module App
    ( AppEnv (..)
    , AppM
    , MonadApp (..)
    , createTodo
    , createTodoWithFields
    , deleteTodo
    , loadTodos
    , runAppM
    , transitionToCancelled
    , transitionToCompleted
    , transitionToInProgress
    , transitionToRegistered
    , updateTodo
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)

import qualified DB

import           Database.SQLite.Simple (Connection)

import           Flow                   ((<|))

import qualified I18n

-- | Application environment containing dependencies (Effectful)
data AppEnv = AppEnv { envConnection :: !Connection
                     , envMessages   :: !I18n.I18nMessages
                     }

-- | MTL-based monad stack (Effectful)
newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }
     deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppEnv)

-- | Type class for application capabilities (Effectful)
class (MonadIO m, MonadReader AppEnv m) => MonadApp m where
    getConnection :: m Connection
    getConnection = asks envConnection

    getMessages :: m I18n.I18nMessages
    getMessages = asks envMessages

instance MonadApp AppM

-- | Run the application monad (Effectful)
runAppM :: AppEnv -> AppM a -> IO a
runAppM env = flip runReaderT env . unAppM

-- | Load all todos from database (Effectful)
loadTodos :: MonadApp m => m [DB.TodoRow]
loadTodos = do
    conn <- getConnection
    liftIO <| DB.getAllTodos conn

-- | Create a new todo with action text only (Effectful)
createTodo :: MonadApp m => String -> m DB.TodoId
createTodo text = do
    conn <- getConnection
    liftIO <| DB.createTodo conn text

-- | Create a new todo with all fields (Effectful)
createTodoWithFields :: MonadApp m => String -> Maybe String -> Maybe String -> Maybe String -> m DB.TodoId
createTodoWithFields action subj indObj dirObj = do
    conn <- getConnection
    liftIO <| DB.createTodoWithFields conn action subj indObj dirObj

-- | Update an existing todo (Effectful)
updateTodo :: MonadApp m => DB.TodoId -> String -> Maybe String -> Maybe String -> Maybe String -> m ()
updateTodo tid action subj indObj dirObj = do
    conn <- getConnection
    liftIO <| DB.updateTodoWithFields conn tid action subj indObj dirObj

-- | Delete a todo (Effectful)
deleteTodo :: MonadApp m => DB.TodoId -> m ()
deleteTodo tid = do
    conn <- getConnection
    liftIO <| DB.deleteTodo conn tid

-- | Transition todo to InProgress (Effectful)
transitionToInProgress :: MonadApp m => DB.TodoId -> m ()
transitionToInProgress tid = do
    conn <- getConnection
    liftIO <| DB.transitionToInProgress conn tid

-- | Transition todo to Cancelled (Effectful)
transitionToCancelled :: MonadApp m => DB.TodoId -> m ()
transitionToCancelled tid = do
    conn <- getConnection
    liftIO <| DB.transitionToCancelled conn tid

-- | Transition todo to Completed (Effectful)
transitionToCompleted :: MonadApp m => DB.TodoId -> m ()
transitionToCompleted tid = do
    conn <- getConnection
    liftIO <| DB.transitionToCompleted conn tid

-- | Transition todo to Registered (Effectful)
transitionToRegistered :: MonadApp m => DB.TodoId -> m ()
transitionToRegistered tid = do
    conn <- getConnection
    liftIO <| DB.transitionToRegistered conn tid
