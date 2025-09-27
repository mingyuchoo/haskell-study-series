module Main
    where

import           DB.Basic           (localConnString, migrateDB)
import qualified DB.Esq             as E

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  choose args
  where
    choose :: [String] -> IO ()
    choose as
      | null as || head as /= "esq" = migrateDB localConnString
      | otherwise                   = E.migrateDB localConnString
