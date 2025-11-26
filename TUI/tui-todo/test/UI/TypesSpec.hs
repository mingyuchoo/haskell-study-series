{-# LANGUAGE OverloadedStrings #-}

module UI.TypesSpec
    ( spec
    ) where

import qualified Brick.Widgets.Edit as E
import           Brick.Widgets.List (list)
import           Flow             ((<|))
import qualified DB

import qualified Data.Vector        as Vec

import           Lens.Micro         ((%~), (&), (.~), (^.))

import           Test.Hspec

import           UI.Types

spec :: Spec
spec = do
  describe "Mode" <| do
    it "ViewMode와 InputMode가 다르게 비교되어야 함" <| do
      ViewMode `shouldNotBe` InputMode

    it "같은 모드는 동일해야 함" <| do
      ViewMode `shouldBe` ViewMode
      InputMode `shouldBe` InputMode

    it "EditMode는 TodoId를 포함해야 함" <| do
      EditMode 1 `shouldBe` EditMode 1
      EditMode 1 `shouldNotBe` EditMode 2

  describe "Name" <| do
    it "모든 Name이 Eq 인스턴스를 가져야 함" <| do
      TodoList `shouldBe` TodoList
      ActionField `shouldBe` ActionField
      SubjectField `shouldBe` SubjectField
      IndirectObjectField `shouldBe` IndirectObjectField
      DirectObjectField `shouldBe` DirectObjectField

    it "다른 Name은 같지 않아야 함" <| do
      TodoList `shouldNotBe` ActionField
      SubjectField `shouldNotBe` DirectObjectField

    it "Name은 Ord 인스턴스를 가져야 함" <| do
      compare TodoList ActionField `shouldSatisfy` const True

  describe "FocusedField" <| do
    it "모든 FocusedField가 Eq 인스턴스를 가져야 함" <| do
      FocusAction `shouldBe` FocusAction
      FocusSubject `shouldBe` FocusSubject
      FocusIndirectObject `shouldBe` FocusIndirectObject
      FocusDirectObject `shouldBe` FocusDirectObject

    it "다른 FocusedField는 같지 않아야 함" <| do
      FocusAction `shouldNotBe` FocusSubject
      FocusIndirectObject `shouldNotBe` FocusDirectObject

  describe "Todo" <| do
    let sampleTodo = Todo 1 "Test action" False "2024-01-01 10:00"
                          (Just "Subject") (Just "Object")
                          (Just "Indirect") (Just "Direct") Nothing

    it "Todo는 Eq 인스턴스를 가져야 함" <| do
      sampleTodo `shouldBe` sampleTodo

    it "todoId 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoId `shouldBe` 1

    it "todoAction 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoAction `shouldBe` "Test action"

    it "todoCompleted 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoCompleted `shouldBe` False

    it "todoCreatedAt 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoCreatedAt `shouldBe` "2024-01-01 10:00"

    it "todoSubject 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoSubject `shouldBe` Just "Subject"

    it "todoObject 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoObject `shouldBe` Just "Object"

    it "todoIndirectObject 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoIndirectObject `shouldBe` Just "Indirect"

    it "todoDirectObject 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoDirectObject `shouldBe` Just "Direct"

    it "todoCompletedAt 렌즈가 올바르게 동작해야 함" <| do
      sampleTodo ^. todoCompletedAt `shouldBe` Nothing

    it "렌즈로 필드를 수정할 수 있어야 함" <| do
      let modified = sampleTodo & todoAction .~ "Modified"
      modified ^. todoAction `shouldBe` "Modified"

    it "todoCompleted를 토글할 수 있어야 함" <| do
      let toggled = sampleTodo & todoCompleted %~ not
      toggled ^. todoCompleted `shouldBe` True

  describe "fromTodoRow" <| do
    it "DB.TodoRow를 UI.Todo로 변환해야 함" <| do
      let dbRow = DB.TodoRow 1 "Action" False "2024-01-01"
                             (Just "Sub") (Just "Obj")
                             (Just "Ind") (Just "Dir") Nothing
          uiTodo = fromTodoRow dbRow
      uiTodo ^. todoId `shouldBe` 1
      uiTodo ^. todoAction `shouldBe` "Action"
      uiTodo ^. todoCompleted `shouldBe` False
      uiTodo ^. todoCreatedAt `shouldBe` "2024-01-01"
      uiTodo ^. todoSubject `shouldBe` Just "Sub"
      uiTodo ^. todoObject `shouldBe` Just "Obj"
      uiTodo ^. todoIndirectObject `shouldBe` Just "Ind"
      uiTodo ^. todoDirectObject `shouldBe` Just "Dir"
      uiTodo ^. todoCompletedAt `shouldBe` Nothing

    it "완료된 TodoRow를 변환해야 함" <| do
      let dbRow = DB.TodoRow 2 "Done" True "2024-01-01"
                             Nothing Nothing Nothing Nothing
                             (Just "2024-01-02")
          uiTodo = fromTodoRow dbRow
      uiTodo ^. todoCompleted `shouldBe` True
      uiTodo ^. todoCompletedAt `shouldBe` Just "2024-01-02"
