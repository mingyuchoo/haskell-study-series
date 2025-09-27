module Main
    where

import           DB.Esq (localConnString, migrateDB)

main :: IO ()
main = migrateDB localConnString
