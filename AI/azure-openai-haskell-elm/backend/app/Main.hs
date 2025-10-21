{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Exception   (SomeException, catch)
import           Control.Monad       (join)

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Flow                ((|>))

import           Presentation.Server

main :: IO ()
main =
    loadConfigFromEnv
        |> fmap runServer
        |> join
        |> flip catch (\e -> ("Error: " <> T.pack (show (e :: SomeException))) |> TIO.putStrLn)
