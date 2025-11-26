{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module App
    ( AppEnv (..)
    , AppM
    , MonadApp (..)
    , createTodo
    , createTodoWithFields
    , deleteTodo
    , loadTodos
    , runAppM
    , toggleTodo
    , updateTodo
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)

import qualified DB
import qualified I18n

import           Database.SQLite.Simple (Connection)

import           Flow                   ((<|))

-- Application environment containing dependencies
data AppEnv = AppEnv
    { envConnection :: !Connection
    , envMessages   :: !I18n.I18nMessages
    }

-- MTL-based monad stack
newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }
     deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppEnv)

-- Type class for application capabilities
class (MonadIO m, MonadReader AppEnv m) => MonadApp m where
    getConnection :: m Connection
    getConnection = asks envConnection

    getMessages :: m I18n.I18nMessages
    getMessages = asks envMessages

instance MonadApp AppM

-- Run the application monad
runAppM :: AppEnv -> AppM a -> IO a
runAppM env = flip runReaderT env . unAppM

-- Load all todos from database
loadTodos :: MonadApp m => m [DB.TodoRow]
loadTodos = do
    conn <- getConnection
    liftIO <| DB.getAllTodos conn

-- Create a new todo with action text only
createTodo :: MonadApp m => String -> m DB.TodoId
createTodo text = do
    conn <- getConnection
    liftIO <| DB.createTodo conn text

-- Create a new todo with all fields
createTodoWithFields :: MonadApp m
                     => String
                     -> Maybe String
                     -> Maybe String
                     -> Maybe String
                     -> m DB.TodoId
createTodoWithFields action subj indObj dirObj = do
    conn <- getConnection
    liftIO <| DB.createTodoWithFields conn action subj indObj dirObj

-- Update an existing todo
updateTodo :: MonadApp m
           => DB.TodoId
           -> String
           -> Maybe String
           -> Maybe String
           -> Maybe String
           -> m ()
updateTodo tid action subj indObj dirObj = do
    conn <- getConnection
    liftIO <| DB.updateTodoWithFields conn tid action subj indObj dirObj

-- Delete a todo
deleteTodo :: MonadApp m => DB.TodoId -> m ()
deleteTodo tid = do
    conn <- getConnection
    liftIO <| DB.deleteTodo conn tid

-- Toggle todo completion status
toggleTodo :: MonadApp m => DB.TodoId -> m ()
toggleTodo tid = do
    conn <- getConnection
    liftIO <| DB.toggleTodoComplete conn tid
