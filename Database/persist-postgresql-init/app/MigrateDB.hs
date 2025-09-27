module Main
    ( main
    ) where

import           DB.Basic           (localConnString, migrateDB)
import qualified DB.Esq             as E

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  choose args
  where
    choose :: [String] -> IO ()
    choose [] = migrateDB localConnString
    choose (a:_)
      | a == "esq" = E.migrateDB localConnString
      | otherwise   = migrateDB localConnString
