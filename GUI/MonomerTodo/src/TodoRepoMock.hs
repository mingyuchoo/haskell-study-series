{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TodoRepoMock
    ( MockM
    , runMockM
    ) where

import           Control.Monad.State

import           TodoRepo

import           TodoTypes

-- | Mock 모나드 (테스트용)
type MockM = State [Todo]

-- | Mock 실행
runMockM :: [Todo] -> MockM a -> (a, [Todo])
runMockM initialState action = runState action initialState

-- | Mock Repository 인스턴스
instance MonadTodoRepo MockM where
  getAllTodos = get

  insertTodo todo = do
    todos <- get
    let newTodos = todo : todos
    put newTodos
    return todo

  updateTodo todoId todo = do
    todos <- get
    let updatedTodos = map (\t -> if _todoId t == fromIntegral todoId then todo else t) todos
    put updatedTodos

  deleteTodo todoId = do
    todos <- get
    let filteredTodos = filter (\t -> _todoId t /= fromIntegral todoId) todos
    put filteredTodos

  initializeDb = return ()
