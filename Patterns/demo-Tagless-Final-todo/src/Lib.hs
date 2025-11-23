{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.IORef

data Todo = Todo { todoId :: Int
                 , title :: Text
                 } deriving (Show, Generic)

class Monad m => Todos m where
  addTodo :: Text -> m Todo
  listTodos :: m [Todo]

newtype App a = App { runApp :: IO a
                    } deriving (Functor, Applicative, Monad)

instance Todos App where
  addTodo t = App $ do
    putStrLn $ "[addTodo] " ++ show t
    pure (Todo 1 t)
  listTodos = App $ do
    pure [Todo 1 "Sample"]

program :: Todos m => m [Todo]
program = do
  _ <- addTodo "Buy milk"
  _ <- addTodo "Write Haskell"
  listTodos

someFunc :: IO ()
someFunc = runApp program >>= print
