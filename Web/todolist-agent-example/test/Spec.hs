module Main (main) where

import Test.Hspec (hspec)
import qualified Todo.Application.CompleteTodoSpec
import qualified Todo.Application.CreateTodoSpec
import qualified Todo.Application.DeleteTodoSpec
import qualified Todo.Domain.TodoSpec
import qualified Todo.Interface.HTTP.TodoApiSpec

main :: IO ()
main = hspec $ do
  Todo.Domain.TodoSpec.spec
  Todo.Application.CreateTodoSpec.spec
  Todo.Application.CompleteTodoSpec.spec
  Todo.Application.DeleteTodoSpec.spec
  Todo.Interface.HTTP.TodoApiSpec.spec
