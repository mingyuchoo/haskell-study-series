module DB.Cache
    where

import           Schema.Basic
import           Schema.Cache               (RedisInfo)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.ByteString       (ByteString)
import           Data.Int              (Int64)
import           Database.Redis
import           Control.Exception     (SomeException, try)

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action

cacheUser :: RedisInfo -> Int64 -> User -> IO ()
cacheUser redisInfo uid user = do
  _ <- try (runRedisAction redisInfo $ setex (pack . show $ uid) 3600 (pack . show $ user)) :: IO (Either SomeException (Either Reply Status))
  return ()

fetchUserRedis :: RedisInfo -> Int64 -> IO (Maybe User)
fetchUserRedis redisInfo uid = do
  eResult <- try (runRedisAction redisInfo $ get (pack . show $ uid)) :: IO (Either SomeException (Either Reply (Maybe ByteString)))
  case eResult of
    Left _ -> return Nothing
    Right result -> case result of
      Right (Just userString) -> return $ Just (read . unpack $ userString)
      _                       -> return Nothing

deleteUserCache :: RedisInfo -> Int64 -> IO ()
deleteUserCache redisInfo uid = do
  _ <- try (runRedisAction redisInfo $ del [pack . show $ uid]) :: IO (Either SomeException (Either Reply Integer))
  return ()
