module Domain.Interfaces.HttpClient
    ( HttpClient (..)
    ) where

import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Text                    (Text)
import           Network.HTTP.Client          (Manager, Request)

-- | Interface for HTTP clients that can handle API requests
class HttpClient c where
    -- | Create a request with API key, URL, and request body
    createRequest :: c             -- ^ HTTP client instance
                 -> String         -- ^ API key
                 -> String         -- ^ URL
                 -> LBS.ByteString -- ^ Request body
                 -> IO Request

    -- | Execute a request and return the response body
    executeRequest :: c             -- ^ HTTP client instance
                   -> Manager       -- ^ HTTP connection manager
                   -> Request       -- ^ Request to execute
                   -> IO LBS.ByteString

    -- | Stream a response through a channel
    streamResponse :: c             -- ^ HTTP client instance
                   -> Manager       -- ^ HTTP connection manager
                   -> Request       -- ^ Request to execute
                   -> IO (TChan (Maybe Text))
