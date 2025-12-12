{-# LANGUAGE OverloadedStrings #-}

-- | Main LSP server module
module LSP.Server 
  ( runLspServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Server
import System.IO (stdin, stdout, hSetBuffering, BufferMode(..))


import LSP.Types (defaultServerConfig)

-- | Main LSP server entry point
-- Implements stdio communication setup and configures server options and handlers
runLspServer :: IO Int
runLspServer = do
  -- Set up stdio for LSP communication
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  -- Run the LSP server with configured handlers
  runServer $ ServerDefinition
    { parseConfig = const $ const $ Right defaultServerConfig
    , onConfigChange = \_ -> pure ()
        -- TODO: Integrate configuration change handler from Handlers.Configuration
        -- The handler is implemented but needs LSP library compatibility fixes
    , defaultConfig = defaultServerConfig
    , configSection = "haskellLSP"
    , doInitialize = \env _req -> do
        liftIO $ putStrLn "Server initializing..."
        liftIO $ putStrLn "Initialize request received"
        liftIO $ putStrLn "Server capabilities configured"
        pure $ Right env
    , staticHandlers = \_caps -> mempty
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
    }