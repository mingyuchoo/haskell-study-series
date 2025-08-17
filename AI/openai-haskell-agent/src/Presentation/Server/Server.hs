{-# LANGUAGE OverloadedStrings #-}

module Presentation.Server.Server
    ( runServer
    ) where

import           Application.Interfaces.ChatApplicationService (ChatApplicationService)

import           Data.Text                                     (pack)
import qualified Data.Text.IO                                  as TIO

import           Flow                                          ((<|))

import           Network.Wai                                   (Application)
import           Network.Wai.Handler.Warp                      (defaultSettings,
                                                                runSettings,
                                                                setLogger,
                                                                setPort)
import           Network.Wai.Logger                            (withStdoutLogger)
import           Network.Wai.Middleware.Cors                   (simpleCors)

import           Presentation.Api.ApiHandler                   (API, apiServer)

import           Servant                                       (Proxy (..),
                                                                serve)

-- | Create the WAI application with CORS support
app :: ChatApplicationService s => s -> Application
app chatAppService = simpleCors <| serve (Proxy :: Proxy API) (apiServer chatAppService)

-- | Run the web server with the specified port and application service
runServer :: ChatApplicationService s => Int -> s -> IO ()
runServer port chatAppService = do
    -- Start the server with standard output logging
    withStdoutLogger <| \appLogger -> do
        -- Configure server settings with port and logger
        let settings = setPort port <| setLogger appLogger defaultSettings

        -- Print server information to console
        TIO.putStrLn <| pack <| "Starting server on port " <> show port
        TIO.putStrLn <| pack <| "API available at http://localhost:" <> show port <> "/api/chat"
        TIO.putStrLn <| pack <| "Health check at http://localhost:" <> show port <> "/health"

        -- Run the server with the configured settings
        runSettings settings (app chatAppService)
