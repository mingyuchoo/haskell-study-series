module Main
    ( main
    ) where

import           Infrastructure.Persistence.PostgreSQL.UserRepositoryImpl (localConnString,
                                                                           migrateDB)

main :: IO ()
main = migrateDB localConnString
