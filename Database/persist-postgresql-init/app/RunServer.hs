module Main
    ( main
    ) where

import qualified Server.Basic       as B
import qualified Server.Cache       as C
import qualified Server.Esq         as E

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  choose args
  where
    choose :: [String] -> IO ()
    choose [] = putStrLn "Running Basic Server" >> B.runServer
    choose (a:_)
      | a == "cache" = putStrLn "Running Cache Server" >> C.runServer
      | otherwise    = putStrLn "Running Esqueleto Server" >> E.runServer
