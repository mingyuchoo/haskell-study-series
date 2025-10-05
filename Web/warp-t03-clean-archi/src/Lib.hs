{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( appRunner
    ) where

import           Control.Exception         (bracket)

import           Flow                      ((<|))

import           Network.Wai.Handler.Warp  (run)

import           Infrastructure.Config.AppConfig    (AppConfig(..), defaultConfig)
import           Infrastructure.Database.Connection (initializeDatabase)
import           Application.UserService            (mkUserService)
import           Adapters.Web.UserWebAdapter        (app)

-- | 애플리케이션 진입: 설정 로드 -> DB 초기화 -> 서비스 조립 -> 웹 서버 시작
appRunner :: IO ()
appRunner = do
  let cfg = defaultConfig
  putStrLn <| "listening on " <> show (port cfg)
  bracket (initializeDatabase cfg) (\_ -> putStrLn "Closing database connection") <| \conn -> do
    let svc = mkUserService conn
    run (port cfg) (app svc)
