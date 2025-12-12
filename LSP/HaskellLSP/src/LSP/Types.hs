{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Core data types for the LSP server
module LSP.Types where

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