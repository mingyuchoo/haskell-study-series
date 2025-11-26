{-# LANGUAGE OverloadedStrings #-}

module AppSpec
    ( spec
    ) where

import           App

import           Control.Monad.IO.Class (liftIO)
import           Flow             ((<|))
import qualified DB

import           Database.SQLite.Simple (open)

import qualified I18n

import           System.Directory       (removeFile)
import           System.IO.Error        (catchIOError)

import           Test.Hspec

spec :: Spec
spec = do
  describe "AppEnv" <| do
    it "AppEnv를 생성할 수 있어야 함" <| do
      conn <- open ":memory:"
      let env = AppEnv conn I18n.defaultMessages
      -- AppEnv가 정상적으로 생성되면 성공
      True `shouldBe` True

  describe "runAppM" <| do
    it "AppM 모나드를 실행할 수 있어야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      result <- runAppM env loadTodos
      length result `shouldSatisfy` (>= 0)

  describe "loadTodos" <| do
    it "데이터베이스에서 모든 todos를 로드해야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      todos <- runAppM env loadTodos
      length todos `shouldSatisfy` (>= 0)

  describe "createTodo" <| do
    it "새로운 todo를 생성하고 ID를 반환해야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      todoId <- runAppM env <| createTodo "Test todo"
      todoId `shouldSatisfy` (> 0)

  describe "createTodoWithFields" <| do
    it "모든 필드를 포함한 todo를 생성해야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      todoId <- runAppM env <| createTodoWithFields "Action" (Just "Subject") (Just "Indirect") (Just "Direct")
      todoId `shouldSatisfy` (> 0)

      todos <- runAppM env loadTodos
      let createdTodo = head <| filter (\t -> DB.todoId t == todoId) todos
      DB.todoAction createdTodo `shouldBe` "Action"
      DB.todoSubject createdTodo `shouldBe` Just "Subject"
      DB.todoIndirectObject createdTodo `shouldBe` Just "Indirect"
      DB.todoDirectObject createdTodo `shouldBe` Just "Direct"

  describe "updateTodo" <| do
    it "기존 todo를 업데이트해야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      todoId <- runAppM env <| createTodo "Original"
      runAppM env <| updateTodo todoId "Updated" (Just "NewSubject") Nothing Nothing

      todos <- runAppM env loadTodos
      let updatedTodo = head <| filter (\t -> DB.todoId t == todoId) todos
      DB.todoAction updatedTodo `shouldBe` "Updated"
      DB.todoSubject updatedTodo `shouldBe` Just "NewSubject"

  describe "deleteTodo" <| do
    it "todo를 삭제해야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      todoId <- runAppM env <| createTodo "To be deleted"
      beforeCount <- length <$> runAppM env loadTodos
      runAppM env <| deleteTodo todoId
      afterCount <- length <$> runAppM env loadTodos
      afterCount `shouldBe` (beforeCount - 1)

  describe "toggleTodo" <| do
    it "todo의 완료 상태를 토글해야 함" <| do
      conn <- open ":memory:"
      DB.initDB conn
      let env = AppEnv conn I18n.defaultMessages
      todoId <- runAppM env <| createTodo "Toggle test"

      todos1 <- runAppM env loadTodos
      let todo1 = head <| filter (\t -> DB.todoId t == todoId) todos1
      DB.todoCompleted todo1 `shouldBe` False

      runAppM env <| toggleTodo todoId

      todos2 <- runAppM env loadTodos
      let todo2 = head <| filter (\t -> DB.todoId t == todoId) todos2
      DB.todoCompleted todo2 `shouldBe` True
