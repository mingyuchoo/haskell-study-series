{-# LANGUAGE OverloadedStrings #-}

module Schema.Cache
    ( RedisInfo
    , localRedisInfo
    ) where

import           Database.Redis (ConnectInfo, defaultConnectInfo)

-- Centralized Redis schema/types used across DB and Server layers

type RedisInfo = ConnectInfo

-- Default local Redis connection info
localRedisInfo :: RedisInfo
localRedisInfo = defaultConnectInfo
