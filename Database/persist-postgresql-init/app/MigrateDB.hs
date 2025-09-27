module Main
    where

import           DB.Basic           (localConnString, migrateDB)
import qualified DB.Esq             as E

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args || head args /= "esq"
    then migrateDB localConnString
    else E.migrateDB localConnString
