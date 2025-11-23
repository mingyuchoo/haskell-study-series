{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Control.Monad.Reader
import Control.Monad.State

data Todo = Todo { todoId :: Int
                 , title :: Text
                 } deriving (Show, Generic)


class Monad m => MonadTodo m where
  addTodo :: Text -> m Todo
  listTodos :: m [Todo]

newtype App a = App { unApp :: StateT [Todo] IO a
                    } deriving (Functor, Applicative, Monad, MonadIO, MonadState [Todo])

instance MonadTodo App where
  addTodo t = do
    todos <- get
    let new = Todo (length todos + 1) t
    put (todos ++ [new])
    return new
  listTodos = get

runApp :: App a -> IO a
runApp app = evalStateT (unApp app) []

someFunc :: IO ()
someFunc = runApp $ do
  _ <- addTodo "Buy milk"
  _ <- addTodo "Write Haskell"
  listTodos >>= liftIO . print
