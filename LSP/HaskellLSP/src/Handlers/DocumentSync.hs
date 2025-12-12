{-# LANGUAGE OverloadedStrings #-}

-- | Document synchronization handlers for LSP server
module Handlers.DocumentSync
  ( handleDidOpen
  , handleDidChange
  , handleDidClose
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Protocol.Message

import LSP.Types (ServerState(..), DocumentState(..), initialServerState)

-- | Handle textDocument/didOpen notification
-- Stores document content in server state and triggers initial diagnostics
handleDidOpen :: DidOpenTextDocumentParams -> LspM ServerState ()
handleDidOpen (DidOpenTextDocumentParams (TextDocumentItem uri _ version content)) = do
  liftIO $ putStrLn $ "Document opened: " ++ show uri
  
  -- TODO: Store document content in server state
  -- TODO: Trigger initial diagnostics analysis
  -- This will be implemented when the diagnostics engine is ready
  pure ()

-- | Handle textDocument/didChange notification  
-- Updates document content with incremental changes and triggers diagnostics update
handleDidChange :: DidChangeTextDocumentParams -> LspM ServerState ()
handleDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri version) changes) = do
  liftIO $ putStrLn $ "Document changed: " ++ show uri ++ " (version " ++ show version ++ ")"
  
  -- Apply changes to get new content
  let newContent = applyChanges "" changes  -- For now, use empty string as base
  liftIO $ putStrLn $ "New content length: " ++ show (Data.Text.length newContent)
  
  -- TODO: Get current document state from server state
  -- TODO: Update server state with new content and version
  -- TODO: Trigger diagnostics update
  -- This will be implemented when the diagnostics engine is ready
  pure ()

-- | Handle textDocument/didClose notification
-- Removes document from server state and clears diagnostics for closed document  
handleDidClose :: DidCloseTextDocumentParams -> LspM ServerState ()
handleDidClose (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = do
  liftIO $ putStrLn $ "Document closed: " ++ show uri
  
  -- TODO: Remove document from server state
  -- TODO: Clear diagnostics for the closed document
  -- This will be implemented when the diagnostics engine is ready
  -- Example: sendNotification SMethod_TextDocumentPublishDiagnostics $
  --   PublishDiagnosticsParams uri Nothing []
  pure ()

-- | Apply text changes to document content
-- For now, we only support full document sync (TextDocumentSyncKind.Full)
-- TODO: Implement incremental sync support
applyChanges :: Text -> [TextDocumentContentChangeEvent] -> Text
applyChanges originalContent changes =
  -- For full document sync, we just take the text from the last change
  case changes of
    [] -> originalContent
    (change:_) -> 
      -- For now, just return the original content
      -- TODO: Properly extract text from TextDocumentContentChangeEvent
      -- This requires understanding the exact structure of the sum type
      originalContent