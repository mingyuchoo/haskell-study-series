{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Lib
import Lens.Micro ((^.), (&), (.~), (%~))
import Brick.Widgets.List (list, listElements, listSelected)
import qualified Brick.Widgets.Edit as E
import qualified Data.Vector as Vec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "trim 함수" $ do
    it "앞뒤 공백을 제거해야 함" $ do
      trim "  hello  " `shouldBe` "hello"
    
    it "중간의 여러 공백을 하나로 줄여야 함" $ do
      trim "hello   world" `shouldBe` "hello world"
    
    it "빈 문자열은 빈 문자열을 반환해야 함" $ do
      trim "" `shouldBe` ""
    
    it "공백만 있는 문자열은 빈 문자열을 반환해야 함" $ do
      trim "   " `shouldBe` ""
    
    it "개행문자와 탭도 처리해야 함" $ do
      trim "\n\thello\t\n" `shouldBe` "hello"

  describe "Todo 데이터 타입" $ do
    let sampleTodo = Todo "Test task" False "2024-01-01 10:00"
    
    it "todoText 렌즈가 올바르게 동작해야 함" $ do
      sampleTodo ^. todoText `shouldBe` "Test task"
    
    it "todoCompleted 렌즈가 올바르게 동작해야 함" $ do
      sampleTodo ^. todoCompleted `shouldBe` False
    
    it "todoCreatedAt 렌즈가 올바르게 동작해야 함" $ do
      sampleTodo ^. todoCreatedAt `shouldBe` "2024-01-01 10:00"
    
    it "todoCompleted를 토글할 수 있어야 함" $ do
      let toggled = sampleTodo & todoCompleted %~ not
      toggled ^. todoCompleted `shouldBe` True

  describe "Mode 타입" $ do
    it "ViewMode와 InputMode가 다르게 비교되어야 함" $ do
      ViewMode `shouldNotBe` InputMode
    
    it "같은 모드는 동일해야 함" $ do
      ViewMode `shouldBe` ViewMode
      InputMode `shouldBe` InputMode

  describe "Name 타입" $ do
    it "TodoList와 InputField가 다르게 비교되어야 함" $ do
      TodoList `shouldNotBe` InputField
    
    it "Name 타입은 Ord 인스턴스를 가져야 함" $ do
      compare TodoList InputField `shouldSatisfy` const True

  describe "AppState 초기화" $ do
    let emptyTodos = Vec.empty
        initialState = AppState
          { _todoList = list TodoList emptyTodos 1
          , _inputEditor = E.editor InputField (Just 1) ""
          , _mode = ViewMode
          }
    
    it "초기 모드는 ViewMode여야 함" $ do
      initialState ^. mode `shouldBe` ViewMode
    
    it "빈 todo 리스트로 시작할 수 있어야 함" $ do
      Vec.null (listElements (initialState ^. todoList)) `shouldBe` True

  describe "Todo 리스트 조작" $ do
    let todo1 = Todo "Task 1" False "2024-01-01"
        todo2 = Todo "Task 2" True "2024-01-02"
        todos = Vec.fromList [todo1, todo2]
        todoListWidget = list TodoList todos 1
    
    it "리스트에 Todo 항목이 포함되어야 함" $ do
      Vec.length (listElements todoListWidget) `shouldBe` 2
    
    it "첫 번째 항목이 선택되어야 함" $ do
      listSelected todoListWidget `shouldBe` Just 0
