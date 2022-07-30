module Library
    where

import           Data.Aeson              (FromJSON (..), ToJSON (..), (.:))
import           RIO                     (Generic, IO, LByteString, Text, (<$>))

import qualified Data.Aeson              as JSON
import qualified Network.HTTP.Client     as HTTP

import           Network.HTTP.Client.TLS (newTlsManager)
import           Prelude

data User = User { userId       :: !Int
                 , userName     :: !Text
                 , userUsername :: !Text
                 , userEmail    :: !Text
                 } deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON = JSON.genericToJSON $ jsonOptions "user"


jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
   in JSON.defaultOptions { JSON.fieldLabelModifier = drop prefixLength
                          }

getUsersContent :: IO LByteString
getUsersContent = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  HTTP.responseBody <$> HTTP.httpLbs request manager


runMain :: IO ()
runMain = do
  manager <- newTlsManager
  putStrLn "Hello, World!"
