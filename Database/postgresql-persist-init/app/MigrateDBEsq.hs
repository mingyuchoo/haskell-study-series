module Main
    ( main
    ) where

import           DB.Esq (localConnString, migrateDB)

main :: IO ()
main = migrateDB localConnString
