{-# LANGUAGE OverloadedStrings #-}

-- | Document synchronization handlers for LSP server
module Handlers.DocumentSync
    ( handleDidChange
    , handleDidClose
    , handleDidOpen
    ) where

import Flow ((<|), (|>))
import           Analysis.Parser               (parseModule)
import qualified Analysis.Parser

import           Control.Monad.IO.Class        (liftIO)

import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           LSP.Diagnostics               (analyzeDiagnostics)
import qualified LSP.Diagnostics               as Diag
import           LSP.Types                     (ServerState (..))

import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server

-- | Handle textDocument/didOpen notification
-- Stores document content in server state and triggers initial diagnostics
handleDidOpen :: DidOpenTextDocumentParams -> LspM ServerState ()
handleDidOpen (DidOpenTextDocumentParams (TextDocumentItem uri _ _version content)) = do
  liftIO $ putStrLn $ "Document opened: " <> show uri

  -- TODO: Store document content in server state
  -- For now, just trigger diagnostics analysis

  -- Trigger initial diagnostics analysis
  analyzeAndPublishDiagnostics uri content

-- | Handle textDocument/didChange notification
-- Updates document content with incremental changes and triggers diagnostics update
handleDidChange :: DidChangeTextDocumentParams -> LspM ServerState ()
handleDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri version) changes) = do
  liftIO $ putStrLn $ "Document changed: " <> show uri <> " (version " <> show version <> ")"

  -- For now, use a simplified approach without state management
  -- Apply changes to get new content (simplified)
  let newContent = applyChanges "" changes  -- Use empty string as base for now
  liftIO $ putStrLn $ "New content length: " <> show (T.length newContent)

  -- Trigger diagnostics update
  analyzeAndPublishDiagnostics uri newContent

-- | Handle textDocument/didClose notification
-- Removes document from server state and clears diagnostics for closed document
handleDidClose :: DidCloseTextDocumentParams -> LspM ServerState ()
handleDidClose (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = do
  liftIO $ putStrLn $ "Document closed: " <> show uri

  -- TODO: Remove document from server state

  -- Clear diagnostics for the closed document
  let clearParams = PublishDiagnosticsParams
        { _uri = uri
        , _version = Nothing
        , _diagnostics = []
        }
  sendNotification SMethod_TextDocumentPublishDiagnostics clearParams

-- | Analyze document content and publish diagnostics to client
analyzeAndPublishDiagnostics :: Uri -> Text -> LspM ServerState ()
analyzeAndPublishDiagnostics uri content = do
  liftIO $ putStrLn $ "Analyzing diagnostics for: " <> show uri

  -- Parse the document and analyze for diagnostics
  case parseModule content of
    Left _parseError -> do
      -- If parsing fails, we'll get diagnostics from the diagnostics engine
      -- which handles parse errors internally
      let emptyModule = Analysis.Parser.ParsedModule
            { Analysis.Parser.pmSource = content
            , Analysis.Parser.pmDeclarations = []
            , Analysis.Parser.pmImports = []
            , Analysis.Parser.pmExports = Nothing
            }

      let diagnosticInfos = analyzeDiagnostics emptyModule

      -- Publish diagnostics to client
      Diag.publishDiagnostics uri diagnosticInfos

    Right parsedModule -> do
      -- Successful parsing - analyze for other issues
      let diagnosticInfos = analyzeDiagnostics parsedModule

      -- Publish diagnostics to client
      Diag.publishDiagnostics uri diagnosticInfos

      liftIO $ putStrLn $ "Published " <> show (length diagnosticInfos) <> " diagnostics"

-- | Apply text changes to document content
-- For now, we only support full document sync (TextDocumentSyncKind.Full)
-- TODO: Implement incremental sync support
applyChanges :: Text -> [TextDocumentContentChangeEvent] -> Text
applyChanges originalContent changes =
  -- For full document sync, we extract the text from the change event
  case changes of
    [] -> originalContent
    (change:_) ->
      -- Extract text from the change event
      -- TextDocumentContentChangeEvent is a sum type, we need to handle both cases
      extractTextFromChange change originalContent

-- | Extract text from TextDocumentContentChangeEvent
-- For now, simplified implementation that just returns the original content
-- TODO: Implement proper change event handling based on LSP specification
extractTextFromChange :: TextDocumentContentChangeEvent -> Text -> Text
extractTextFromChange _change originalContent = originalContent
