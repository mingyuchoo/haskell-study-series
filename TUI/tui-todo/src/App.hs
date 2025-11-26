{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module App
    ( AppEnv (..)
    , AppM
    , runAppM
    , getConnection
    , loadTodosFromDB
    , saveTodoToDB
    , deleteTodoFromDB
    , toggleTodoInDB
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)
import           Database.SQLite.Simple (Connection)

import qualified Data.Vector            as Vec
import qualified DB

-- 애플리케이션 환경 (의존성)
data AppEnv = AppEnv
    { envConnection :: Connection
    }

-- MTL 기반 모나드 스택
newtype AppM a = AppM
    { unAppM :: ReaderT AppEnv IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

-- AppM 실행
runAppM :: AppEnv -> AppM a -> IO a
runAppM env app = runReaderT (unAppM app) env

-- 데이터베이스 연결 가져오기
getConnection :: AppM Connection
getConnection = asks envConnection

-- 데이터베이스에서 Todo 로드
loadTodosFromDB :: AppM (Vec.Vector (DB.TodoId, String, Bool, String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String))
loadTodosFromDB = do
    conn <- getConnection
    todos <- liftIO $ DB.getAllTodos conn
    return $ Vec.fromList $ map (\t -> (DB.todoId t, DB.todoAction t, DB.todoCompleted t, DB.todoCreatedAt t,
                                        DB.todoSubject t, DB.todoObject t, DB.todoIndirectObject t, 
                                        DB.todoDirectObject t, DB.todoCompletedAt t)) todos

-- 데이터베이스에 Todo 저장
saveTodoToDB :: String -> AppM DB.TodoId
saveTodoToDB text = do
    conn <- getConnection
    liftIO $ DB.createTodo conn text

-- 데이터베이스에서 Todo 삭제
deleteTodoFromDB :: DB.TodoId -> AppM ()
deleteTodoFromDB tid = do
    conn <- getConnection
    liftIO $ DB.deleteTodo conn tid

-- 데이터베이스에서 Todo 완료 상태 토글
toggleTodoInDB :: DB.TodoId -> AppM ()
toggleTodoInDB tid = do
    conn <- getConnection
    liftIO $ DB.toggleTodoComplete conn tid
