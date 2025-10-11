{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Exception   (SomeException, catch)

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Presentation.Server

main :: IO ()
main = do
    config <- loadConfigFromEnv
    runServer config
        `catch` \e -> TIO.putStrLn $ "Error: " <> T.pack (show (e :: SomeException))
