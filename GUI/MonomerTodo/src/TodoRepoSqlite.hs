{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module TodoRepoSqlite
    ( AppM
    , SqliteEnv (..)
    , runAppM
    , withSqliteEnv
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow

import           TodoRepo

import           TodoTypes

-- | SQLite 환경 설정
data SqliteEnv = SqliteEnv { sqliteConn :: Connection
                           }

-- | 애플리케이션 모나드
type AppM = ReaderT SqliteEnv IO

-- | AppM 실행
runAppM :: SqliteEnv -> AppM a -> IO a
runAppM env action = runReaderT action env

-- | SQLite 환경 생성 및 정리
withSqliteEnv :: FilePath -> (SqliteEnv -> IO a) -> IO a
withSqliteEnv dbPath action = do
  conn <- open dbPath
  let env = SqliteEnv conn
  result <- action env
  close conn
  return result

-- | SQLite Row 인스턴스
instance FromRow Todo where
  fromRow = Todo
    <$> (fromIntegral <$> (field :: RowParser Int64))  -- todoId
    <*> (toTodoType <$> field)  -- todoType (Int로 저장)
    <*> (toTodoStatus <$> field)  -- status (Int로 저장)
    <*> field  -- description

instance ToRow Todo where
  toRow todo = toRow
    ( fromIntegral (_todoId todo) :: Int64
    , fromTodoType (_todoType todo)
    , fromTodoStatus (_status todo)
    , _description todo
    )

-- | Enum 변환 헬퍼
toTodoType :: Int -> TodoType
toTodoType 0 = Home
toTodoType 1 = Work
toTodoType 2 = Sports
toTodoType _ = Home

fromTodoType :: TodoType -> Int
fromTodoType Home   = 0
fromTodoType Work   = 1
fromTodoType Sports = 2

toTodoStatus :: Int -> TodoStatus
toTodoStatus 0 = Pending
toTodoStatus 1 = Done
toTodoStatus _ = Pending

fromTodoStatus :: TodoStatus -> Int
fromTodoStatus Pending = 0
fromTodoStatus Done    = 1

-- | MonadTodoRepo 인스턴스
instance MonadTodoRepo AppM where
  getAllTodos = do
    conn <- asks sqliteConn
    liftIO $ query_ conn "SELECT id, type, status, description FROM todos ORDER BY id DESC"

  insertTodo todo = do
    conn <- asks sqliteConn
    liftIO $ do
      execute conn
        "INSERT INTO todos (id, type, status, description) VALUES (?, ?, ?, ?)"
        ( fromIntegral (_todoId todo) :: Int64
        , fromTodoType (_todoType todo)
        , fromTodoStatus (_status todo)
        , _description todo
        )
      return todo

  updateTodo todoId todo = do
    conn <- asks sqliteConn
    liftIO $ execute conn
      "UPDATE todos SET type = ?, status = ?, description = ? WHERE id = ?"
      ( fromTodoType (_todoType todo)
      , fromTodoStatus (_status todo)
      , _description todo
      , fromIntegral todoId :: Int64
      )

  deleteTodo todoId = do
    conn <- asks sqliteConn
    liftIO $ execute conn "DELETE FROM todos WHERE id = ?" (Only (fromIntegral todoId :: Int64))

  initializeDb = do
    conn <- asks sqliteConn
    liftIO $ execute_ conn $ Query $ T.unlines
      [ "CREATE TABLE IF NOT EXISTS todos ("
      , "  id INTEGER PRIMARY KEY,"
      , "  type INTEGER NOT NULL,"
      , "  status INTEGER NOT NULL,"
      , "  description TEXT NOT NULL"
      , ")"
      ]
