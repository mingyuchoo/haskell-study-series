{-# LANGUAGE BangPatterns #-}

module Cache
    ( Cache
    , newCache
    , lookup
    , insert
    , clear
    ) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Control.Concurrent.STM

-- | 캐시 타입 (STM 기반)
newtype Cache k v = Cache (TVar (M.Map k v))

-- | 새 캐시 생성
newCache :: IO (Cache k v)
newCache = Cache <$> newTVarIO M.empty

-- | 캐시에서 조회
lookup :: Ord k => k -> Cache k v -> IO (Maybe v)
lookup key (Cache tvar) = atomically $ do
    cache <- readTVar tvar
    return $ M.lookup key cache

-- | 캐시에 삽입
insert :: Ord k => k -> v -> Cache k v -> IO ()
insert key value (Cache tvar) = atomically $ do
    modifyTVar' tvar (M.insert key value)

-- | 캐시 초기화
clear :: Cache k v -> IO ()
clear (Cache tvar) = atomically $ writeTVar tvar M.empty
