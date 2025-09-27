{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module DB.Esq
    ( PGInfo
    , localConnString
    , fetchAllUsersPG
    , fetchArticlePG
    , fetchArticlesByAuthorPG
    , fetchRecentArticlesPG
    , fetchUserPG
    , migrateDB
    , patchUserPG
    , replaceUserPG
    , createUserPG
    , createArticlePG
    , deleteArticlePG
    , deleteUserPG
    )
    where

import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              filterLogger,
                                              runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Int                    (Int64)
import           Data.Maybe                  (catMaybes, listToMaybe)
import           Database.Esqueleto.Legacy  (InnerJoin (..), desc, from, limit,
                                              on, orderBy, select, val, where_,
                                              (==.), (^.))
import           Database.Persist            (Entity, delete, entityVal, get,
                                              insert, replace, update, (=.))
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)
import           Database.Persist.Sql        (fromSqlKey, toSqlKey)
import           Schema.Esq

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

fetchAllUsersPG :: PGInfo -> IO [Entity User]
fetchAllUsersPG connString = runAction connString selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) [Entity User]
    selectAction = select . from $ \users -> do
      return users

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

-- Replace all fields of a user for PUT /users/{id}. Returns True if user existed and was replaced.
replaceUserPG :: PGInfo -> Int64 -> User -> IO Bool
replaceUserPG connString uid user = runAction connString action
  where
    key = toSqlKey uid
    action = do
      mExisting <- get key
      handleReplace mExisting
    handleReplace x
      | Nothing <- x = pure False
      | Just _  <- x = replace key user >> pure True

-- Partially update fields for PATCH /users/{id}. Returns True if user existed and was updated.
patchUserPG :: PGInfo -> Int64 -> UserPatch -> IO Bool
patchUserPG connString uid up = runAction connString action
  where
    key = toSqlKey uid
    action = do
      mExisting <- get key
      handlePatch mExisting
    handlePatch x
      | Nothing <- x = pure False
      | Just _  <- x =
          let sets = catMaybes
                [ (UserName =.)       <$> upName up
                , (UserEmail =.)      <$> upEmail up
                , (UserAge =.)        <$> upAge up
                , (UserOccupation =.) <$> upOccupation up
                ]
          in handleSets sets
    handleSets s
      | [] <- s = pure True -- nothing to update but treat as success
      | _  <- s = update key s >> pure True

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

fetchArticlePG :: PGInfo -> Int64 -> IO (Maybe Article)
fetchArticlePG connString aid = runAction connString selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe Article)
    selectAction = ((fmap entityVal) . listToMaybe) <$> (select . from $ \articles -> do
      where_ (articles ^. ArticleId ==. val (toSqlKey aid))
      return articles)

fetchArticlesByAuthorPG :: PGInfo -> Int64 -> IO [Entity Article]
fetchArticlesByAuthorPG connString uid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Article]
    fetchAction = select . from $ \articles -> do
      where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return articles

fetchRecentArticlesPG :: PGInfo -> IO [(Entity User, Entity Article)]
fetchRecentArticlesPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity User, Entity Article)]
    fetchAction = select . from $ \(users `InnerJoin` articles) -> do
      on (users ^. UserId ==. articles ^. ArticleAuthorId)
      orderBy [desc (articles ^. ArticlePublishedTime)]
      limit 3
      return (users, articles)

createArticlePG :: PGInfo -> Article -> IO Int64
createArticlePG connString article = fromSqlKey <$> runAction connString (insert article)

deleteArticlePG :: PGInfo -> Int64 -> IO ()
deleteArticlePG connString aid = runAction connString (delete articleKey)
  where
    articleKey :: Key Article
    articleKey = toSqlKey aid
