{-# LANGUAGE OverloadedStrings #-}

module Todo.Application.DeleteTodoSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Todo.Application.CreateTodo as Create
import qualified Todo.Application.DeleteTodo as Delete
import Todo.Application.TodoRepository (TodoRepository (..))
import Support.Fixtures (fixedTime, fixedTodoId, newInMemoryRepository)

spec :: Spec
spec = describe "DeleteTodo.execute" $ do
  it "deletes an existing Todo" $ do
    repo <- newInMemoryRepository
    let createDeps = Create.CreateTodoDependencies repo (pure fixedTodoId) (pure fixedTime)
    _ <- Create.execute createDeps (Create.CreateTodoCommand "Buy milk" Nothing)
    result <- Delete.execute repo fixedTodoId
    result `shouldBe` Right ()
    stored <- findTodoById repo fixedTodoId
    stored `shouldBe` Right Nothing

  it "reports a missing Todo" $ do
    repo <- newInMemoryRepository
    result <- Delete.execute repo fixedTodoId
    result `shouldBe` Left Delete.DeleteTodoNotFound
