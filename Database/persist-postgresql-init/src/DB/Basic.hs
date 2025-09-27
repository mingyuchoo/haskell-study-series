{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Basic
    where

import           Schema.Basic
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Reader        (runReaderT)
import           Data.Int                    (Int64)
import           Data.Maybe                  (catMaybes)
import           Database.Persist
import           Database.Persist.Postgresql

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

selectYoungTeachers :: (MonadIO m) => SqlPersistT m [Entity User]
selectYoungTeachers = selectList [UserAge <. 25, UserOccupation ==. "Teacher"] []

selectYoungTeachers' :: (MonadIO m) => SqlPersistT m [Entity User]
selectYoungTeachers' = selectList
  [UserAge <. 25, UserOccupation ==. "Teacher"] [Asc UserEmail, OffsetBy 5, LimitTo 100]

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

fetchAllUsersPG :: PGInfo -> IO [Entity User]
fetchAllUsersPG connString = runAction connString (selectList [] [])

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

-- Replace full user document (PUT)
putUserPG :: PGInfo -> Int64 -> User -> IO ()
putUserPG connString uid user = runAction connString (replace userKey user)
  where
    userKey :: Key User
    userKey = toSqlKey uid

-- Partial update user (PATCH)
patchUserPG :: PGInfo -> Int64 -> UpdateUser -> IO ()
patchUserPG connString uid UpdateUser{..} = runAction connString (update userKey updates)
  where
    userKey :: Key User
    userKey = toSqlKey uid
    updates :: [Update User]
    updates = catMaybes
      [ (UserName =.) <$> uuName
      , (UserEmail =.) <$> uuEmail
      , (UserAge =.) <$> uuAge
      , (UserOccupation =.) <$> uuOccupation
      ]
