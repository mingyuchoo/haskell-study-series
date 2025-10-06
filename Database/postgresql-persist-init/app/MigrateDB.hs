module Main
    ( main
    ) where

import           Infrastructure.Persistence.PostgreSQL.UserRepositoryImpl (localConnString,
                                                                           migrateDB)

import           System.Environment                                       (getArgs)

main :: IO ()
main = do
  args <- getArgs
  choose args
  where
    choose :: [String] -> IO ()
    choose [] = migrateDB localConnString
    choose (a:_)
      | a == "esq" = migrateDB localConnString  -- Using same migration for now
      | otherwise   = migrateDB localConnString
