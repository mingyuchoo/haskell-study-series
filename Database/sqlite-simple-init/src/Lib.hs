module Lib
    ( User (..)
    , startApp
    ) where

import           Domain.UserModel                         (User (..))
import           Infrastructure.Repository.UserRepository (UserRepository (..),
                                                           initDB)
import           Infrastructure.Web.Server                (startServer)

-- | Main application entry point that composes all layers
startApp :: IO ()
startApp = do
    conn <- initDB
    let repository = UserRepository conn
    startServer repository
