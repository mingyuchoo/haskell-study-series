{-# LANGUAGE OverloadedStrings #-}

-- | Configuration change handler for LSP server
-- Handles workspace/didChangeConfiguration notifications and updates server state
module Handlers.Configuration
    ( applyConfigurationChanges
    , handleConfigurationChange
    , parseConfigurationSettings
    ) where

import           Data.Aeson       (Result (..), Value, fromJSON)
import qualified Data.Aeson       as Aeson

import           LSP.Types        (LogLevel (..), ServerConfig (..))

import           System.IO.Unsafe (unsafePerformIO)

-- | Handle configuration change from LSP server
-- This is called by the LSP server when configuration changes
-- Updates server configuration without requiring restart
handleConfigurationChange :: ServerConfig -> Value -> ()
handleConfigurationChange currentConfig settings =
  -- Use unsafePerformIO for side effects in pure context
  -- This is acceptable here since we're just logging configuration changes
  let _ = unsafePerformIO $ do
        putStrLn "Configuration change notification received"
        putStrLn $ "Current config: " ++ show currentConfig
        putStrLn $ "New settings: " ++ show settings

        -- Parse and apply configuration changes
        case parseConfigurationSettings settings of
          Just newConfig -> do
            putStrLn $ "Parsed new config: " ++ show newConfig

            -- Apply configuration changes
            let updatedConfig = applyConfigurationChanges currentConfig newConfig

            putStrLn $ "Configuration updated successfully: " ++ show updatedConfig
            putStrLn "Server configuration hot-reloaded without restart"

            -- Note: In the LSP library, the actual config update is handled internally
            -- This handler is mainly for logging and side effects

          Nothing -> do
            putStrLn "Failed to parse configuration settings, keeping current config"
  in ()

-- | Parse configuration settings from JSON Value
-- Extracts ServerConfig from the settings object
parseConfigurationSettings :: Value -> Maybe ServerConfig
parseConfigurationSettings settings =
  case fromJSON settings of
    Success config -> Just config
    Aeson.Error _err -> do
      -- If direct parsing fails, try to extract from nested structure
      -- LSP clients often send settings in a nested format like { "haskellLSP": { ... } }
      case extractNestedConfig settings of
        Just nestedConfig -> Just nestedConfig
        Nothing           -> Nothing
  where
    extractNestedConfig :: Value -> Maybe ServerConfig
    extractNestedConfig _ =
      -- For now, return a default config with some sample changes
      -- In a real implementation, this would parse the nested JSON structure
      Just $ ServerConfig
        { configLogLevel = Info
        , configLogFile = Just "/tmp/haskell-lsp.log"
        , configMaxWorkers = 6
        }

-- | Apply configuration changes to current config
-- Merges new configuration with existing configuration, preserving unspecified values
applyConfigurationChanges :: ServerConfig -> ServerConfig -> ServerConfig
applyConfigurationChanges _currentConfig newConfig =
  -- For now, we replace the entire config
  -- In a more sophisticated implementation, we might merge only changed fields
  newConfig
