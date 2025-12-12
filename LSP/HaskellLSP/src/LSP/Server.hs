{-# LANGUAGE OverloadedStrings #-}

-- | Main LSP server module
module LSP.Server
    ( runLspServer
    ) where

import Flow ((<|), (|>))
import           Control.Monad.IO.Class      (liftIO)

import           LSP.Types                   (defaultServerConfig)

import           Language.LSP.Protocol.Types
import           Language.LSP.Server

import           System.IO                   (BufferMode (..), hSetBuffering,
                                              stdin, stdout)

-- | Main LSP server entry point
-- Implements stdio communication setup and configures server options and handlers
runLspServer :: IO Int
runLspServer = do
  -- Set up stdio for LSP communication
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  liftIO <| putStrLn "Starting Haskell LSP Server..."

  -- Run the LSP server with configured handlers
  runServer <| ServerDefinition
    { parseConfig = const <| const <| Right defaultServerConfig
    , onConfigChange = \_ -> pure ()
    , defaultConfig = defaultServerConfig
    , configSection = "haskellLSP"
    , doInitialize = \env _req -> do
        liftIO <| putStrLn "Server initializing..."
        liftIO <| putStrLn "Initialize request received"
        liftIO <| putStrLn "Server capabilities configured"
        pure <| Right env
    , staticHandlers = \_caps -> mempty  -- Start with empty handlers for now
    , interpretHandler = \env -> Iso (\f -> runLspT env f) liftIO
    , options = defaultOptions
    }

-- | Server capabilities configuration
-- Declares what features this LSP server supports
-- Currently unused but kept for future use
_serverCapabilities :: ServerCapabilities
_serverCapabilities = ServerCapabilities
  { _positionEncoding = Nothing
  , _textDocumentSync = Just <| InL <| TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TextDocumentSyncKind_Incremental
      , _willSave = Nothing
      , _willSaveWaitUntil = Nothing
      , _save = Just <| InR <| SaveOptions { _includeText = Just False }
      }
  , _notebookDocumentSync = Nothing
  , _completionProvider = Just <| CompletionOptions
      { _triggerCharacters = Just ["."]
      , _allCommitCharacters = Nothing
      , _resolveProvider = Just False
      , _completionItem = Nothing
      , _workDoneProgress = Nothing
      }
  , _hoverProvider = Just <| InL True
  , _signatureHelpProvider = Nothing
  , _declarationProvider = Nothing
  , _definitionProvider = Just <| InL True
  , _typeDefinitionProvider = Nothing
  , _implementationProvider = Nothing
  , _referencesProvider = Nothing
  , _documentHighlightProvider = Nothing
  , _documentSymbolProvider = Just <| InL True
  , _codeActionProvider = Nothing
  , _codeLensProvider = Nothing
  , _documentLinkProvider = Nothing
  , _colorProvider = Nothing
  , _documentFormattingProvider = Nothing
  , _documentRangeFormattingProvider = Nothing
  , _documentOnTypeFormattingProvider = Nothing
  , _renameProvider = Nothing
  , _foldingRangeProvider = Nothing
  , _executeCommandProvider = Nothing
  , _selectionRangeProvider = Nothing
  , _linkedEditingRangeProvider = Nothing
  , _callHierarchyProvider = Nothing
  , _semanticTokensProvider = Nothing
  , _monikerProvider = Nothing
  , _typeHierarchyProvider = Nothing
  , _inlineValueProvider = Nothing
  , _inlayHintProvider = Nothing
  , _diagnosticProvider = Nothing
  , _workspaceSymbolProvider = Nothing
  , _workspace = Nothing
  , _experimental = Nothing
  }
