{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Core data types for the LSP server
module LSP.Types 
  ( -- * Core Types
    RequestId
  , Method
  , LspMessage(..)
  , ResponseError(..)
    -- * Error Handling
  , LspErrorCode(..)
  , ErrorSeverity(..)
  , ErrorRecovery(..)
  , lspErrorCodeToInt
  , intToLspErrorCode
    -- * Error Builders
  , mkParseError
  , mkInvalidRequest
  , mkMethodNotFound
  , mkInvalidParams
  , mkInternalError
  , mkServerNotInitialized
  , mkRequestCancelled
    -- * Configuration
  , LogLevel(..)
  , ServerConfig(..)
  , defaultServerConfig
    -- * Document State
  , DocumentState(..)
  , ParsedModule(..)
  , Declaration(..)
  , Import(..)
  , Export(..)
  , SymbolKind(..)
  , Position(..)
  , Range(..)
  , ServerState(..)
  , initialServerState
    -- * Protocol Helpers
  , encodeLspMessage
  , decodeLspMessage
  , parseContentLength
  , extractJsonContent
  , parseJsonRpcMessage
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, encode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.LSP.Protocol.Types (Uri, Diagnostic)

-- | Request ID for JSON-RPC messages
type RequestId = Value

-- | Method name for JSON-RPC messages  
type Method = Text

-- | JSON-RPC error response
data ResponseError = ResponseError
  { errorCode    :: Int
  , errorMessage :: Text
  , errorData    :: Maybe Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Standard JSON-RPC error codes
data LspErrorCode
  = ParseError          -- ^ -32700: Invalid JSON was received by the server
  | InvalidRequest      -- ^ -32600: The JSON sent is not a valid Request object
  | MethodNotFound      -- ^ -32601: The method does not exist / is not available
  | InvalidParams       -- ^ -32602: Invalid method parameter(s)
  | InternalError       -- ^ -32603: Internal JSON-RPC error
  | ServerNotInitialized -- ^ -32002: Server has not been initialized
  | UnknownErrorCode    -- ^ -32001: Unknown error code
  | RequestCancelled    -- ^ -32800: Request was cancelled
  | ContentModified     -- ^ -32801: Content was modified
  deriving (Show, Eq, Generic)

-- | Convert LspErrorCode to integer
lspErrorCodeToInt :: LspErrorCode -> Int
lspErrorCodeToInt ParseError = -32700
lspErrorCodeToInt InvalidRequest = -32600
lspErrorCodeToInt MethodNotFound = -32601
lspErrorCodeToInt InvalidParams = -32602
lspErrorCodeToInt InternalError = -32603
lspErrorCodeToInt ServerNotInitialized = -32002
lspErrorCodeToInt UnknownErrorCode = -32001
lspErrorCodeToInt RequestCancelled = -32800
lspErrorCodeToInt ContentModified = -32801

-- | Convert integer to LspErrorCode
intToLspErrorCode :: Int -> LspErrorCode
intToLspErrorCode (-32700) = ParseError
intToLspErrorCode (-32600) = InvalidRequest
intToLspErrorCode (-32601) = MethodNotFound
intToLspErrorCode (-32602) = InvalidParams
intToLspErrorCode (-32603) = InternalError
intToLspErrorCode (-32002) = ServerNotInitialized
intToLspErrorCode (-32001) = UnknownErrorCode
intToLspErrorCode (-32800) = RequestCancelled
intToLspErrorCode (-32801) = ContentModified
intToLspErrorCode _ = UnknownErrorCode

-- | Error response builders
mkParseError :: Text -> ResponseError
mkParseError msg = ResponseError
  { errorCode = lspErrorCodeToInt ParseError
  , errorMessage = msg
  , errorData = Nothing
  }

mkInvalidRequest :: Text -> ResponseError
mkInvalidRequest msg = ResponseError
  { errorCode = lspErrorCodeToInt InvalidRequest
  , errorMessage = msg
  , errorData = Nothing
  }

mkMethodNotFound :: Text -> ResponseError
mkMethodNotFound method = ResponseError
  { errorCode = lspErrorCodeToInt MethodNotFound
  , errorMessage = "Method not found: " <> method
  , errorData = Nothing
  }

mkInvalidParams :: Text -> ResponseError
mkInvalidParams msg = ResponseError
  { errorCode = lspErrorCodeToInt InvalidParams
  , errorMessage = msg
  , errorData = Nothing
  }

mkInternalError :: Text -> ResponseError
mkInternalError msg = ResponseError
  { errorCode = lspErrorCodeToInt InternalError
  , errorMessage = msg
  , errorData = Nothing
  }

mkServerNotInitialized :: ResponseError
mkServerNotInitialized = ResponseError
  { errorCode = lspErrorCodeToInt ServerNotInitialized
  , errorMessage = "Server not initialized"
  , errorData = Nothing
  }

mkRequestCancelled :: RequestId -> ResponseError
mkRequestCancelled _ = ResponseError
  { errorCode = lspErrorCodeToInt RequestCancelled
  , errorMessage = "Request was cancelled"
  , errorData = Nothing
  }

-- | LSP message wrapper for JSON-RPC communication
data LspMessage
  = RequestMessage RequestId Method Value
  | ResponseMessage RequestId (Either ResponseError Value)
  | NotificationMessage Method Value
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Log levels for server configuration
data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Server configuration settings
data ServerConfig = ServerConfig
  { configLogLevel    :: LogLevel
  , configLogFile     :: Maybe FilePath
  , configMaxWorkers  :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { configLogLevel = Info
  , configLogFile = Nothing
  , configMaxWorkers = 4
  }

-- | State of a single document
data DocumentState = DocumentState
  { docContent    :: Text
  , docVersion    :: Int32
  , docParsed     :: Maybe ParsedModule
  } deriving (Show, Eq, Generic)

-- | Placeholder for parsed module representation
data ParsedModule = ParsedModule
  { pmSource      :: Text
  , pmDeclarations :: [Declaration]
  , pmImports     :: [Import]
  , pmExports     :: Maybe [Export]
  } deriving (Show, Eq, Generic)

-- | Placeholder for declaration representation
data Declaration = Declaration
  { declName      :: Text
  , declType      :: Maybe Text
  , declKind      :: SymbolKind
  , declRange     :: Range
  , declChildren  :: [Declaration]
  } deriving (Show, Eq, Generic)

-- | Placeholder for import representation
data Import = Import
  { importModule :: Text
  , importQualified :: Bool
  , importAs :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Placeholder for export representation  
data Export = Export
  { exportName :: Text
  , exportType :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Symbol kinds for declarations
data SymbolKind = 
    SkFunction
  | SkType
  | SkClass
  | SkInstance
  | SkVariable
  deriving (Show, Eq, Generic)

-- | Position in a document
data Position = Position
  { posLine      :: Int32
  , posCharacter :: Int32
  } deriving (Show, Eq, Ord, Generic)

-- | Range in a document
data Range = Range
  { rangeStart :: Position
  , rangeEnd   :: Position
  } deriving (Show, Eq, Generic)

-- | Error classification for recovery strategies
data ErrorSeverity
  = Recoverable    -- ^ Error that can be handled and processing can continue
  | Fatal          -- ^ Error that requires server shutdown
  | Transient      -- ^ Temporary error that may succeed on retry
  deriving (Show, Eq, Generic)

-- | Error recovery configuration
data ErrorRecovery = ErrorRecovery
  { maxRetries      :: Int
  , retryDelay      :: Int  -- ^ milliseconds
  , fallbackAction  :: IO ()
  }

-- | Overall server state
data ServerState = ServerState
  { stateDocuments    :: Map Uri DocumentState
  , stateConfig       :: ServerConfig
  , stateDiagnostics  :: Map Uri [Diagnostic]
  } deriving (Show, Generic)

-- | Initial server state
initialServerState :: ServerConfig -> ServerState
initialServerState config = ServerState
  { stateDocuments = mempty
  , stateConfig = config
  , stateDiagnostics = mempty
  }

-- | JSON-RPC Protocol Helpers

-- | Encode an LSP message to JSON-RPC format with Content-Length header
encodeLspMessage :: LspMessage -> ByteString
encodeLspMessage msg = 
  let jsonBytes = encode msg
      contentLength = LBS.length jsonBytes
      header = L8.pack $ "Content-Length: " ++ show contentLength ++ "\r\n\r\n"
  in header <> jsonBytes

-- | Decode a JSON-RPC message from ByteString
decodeLspMessage :: ByteString -> Maybe LspMessage
decodeLspMessage = decode

-- | Parse Content-Length header from incoming data
parseContentLength :: ByteString -> Maybe Int
parseContentLength input = 
  case L8.lines input of
    [] -> Nothing
    (firstLine:_) -> 
      let headerStr = L8.unpack firstLine
          prefix = "Content-Length: "
      in if prefix `isPrefixOf` headerStr
         then case reads (drop (length prefix) headerStr) of
                [(len, rest)] | all (`elem` (" \r\n" :: String)) rest -> Just len
                _ -> Nothing
         else Nothing

-- | Extract JSON content after Content-Length header
extractJsonContent :: ByteString -> Maybe ByteString
extractJsonContent input =
  let inputStr = L8.unpack input
      separator = "\r\n\r\n"
  in case splitOn separator inputStr of
       (_:rest:_) -> Just (L8.pack rest)
       _ -> Nothing
  where
    splitOn :: String -> String -> [String]
    splitOn _ [] = [""]
    splitOn delim str = 
      let (before, remainder) = breakOn delim str
      in before : case remainder of
                    [] -> []
                    x -> if delim `isPrefixOf` x
                         then splitOn delim (drop (length delim) x)
                         else [x]
    
    breakOn :: String -> String -> (String, String)
    breakOn _ [] = ([], [])
    breakOn delim str@(c:cs)
      | delim `isPrefixOf` str = ([], str)
      | otherwise = let (before, after) = breakOn delim cs
                    in (c:before, after)

-- | Parse a complete JSON-RPC message with Content-Length header
parseJsonRpcMessage :: ByteString -> Maybe LspMessage
parseJsonRpcMessage input = do
  contentLength <- parseContentLength input
  jsonContent <- extractJsonContent input
  if LBS.length jsonContent >= fromIntegral contentLength
    then decodeLspMessage (LBS.take (fromIntegral contentLength) jsonContent)
    else Nothing