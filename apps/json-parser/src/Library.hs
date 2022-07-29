module Library
    where

import           Data.Aeson              (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson              as JSON
import qualified Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS (newTlsManager)
import           Prelude
import           RIO                     (Generic, IO, LByteString, Text, (<$>))

data User = User { userId       :: !Int
                 , userName     :: !Text
                 , userUsername :: !Text
                 , userEmail    :: !Text
                 } deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON = JSON.genericToJSON JSON.defaultOptions

getUsersContent :: IO LByteString
getUsersContent = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  HTTP.responseBody <$> HTTP.httpLbs request manager

runMain :: IO ()
runMain = do
  manager <- newTlsManager
  putStrLn "Hello, World!"
