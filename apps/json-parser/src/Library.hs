module Library
    where

import           Data.Aeson              (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson              as JSON
import           Data.Char               (toLower)
import qualified Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS (newTlsManager)
import           Prelude
import           RIO
    ( Generic
    , IO
    , LByteString
    , Text
    , (<$>)
    , (>>>)
    )

data User = User { userId       :: !Int
                 , userName     :: !Text
                 , userUsername :: !Text
                 , userEmail    :: !Text
                 , userAddress  :: !Address
                 , userPhone    :: !Text
                 , userWebsite  :: !Text
                 , userCompany  :: !Company
                 } deriving (Eq, Show, Generic)


instance FromJSON User where
  parseJSON = JSON.genericParseJSON $ jsonOptions "user"

-- instance ToJSON User where
--   toJSON = JSON.genericToJSON $ jsonOptions "user"

data Address = Address { addressStreet  :: !Text
                       , addressSuite   :: !Text
                       , addressCity    :: !Text
                       , addressZipcode :: !Text
                       , addressGeo     :: !Geo
                       } deriving (Eq, Show, Generic)

instance FromJSON Address where
  parseJSON = JSON.genericParseJSON $ jsonOptions "address"


data Geo = Geo { geoLat :: !Float
               , geoLng :: !Float
               } deriving (Eq, Show, Generic)

instance FromJSON Geo where
  parseJSON =  JSON.withObject "Geo" $ \x -> do
    latString <- x .: "lat"
    lngString <- x .: "lng"
    let geoLat = read latString
        geoLng = read lngString
    return Geo {geoLat, geoLng}

data Company = Company { companyName        :: !Text
                       , companyCatchPhrase :: !Text
                       , companyBs          :: !Text
                       } deriving (Eq, Show, Generic)

instance FromJSON Company where
  parseJSON = JSON.genericParseJSON $ jsonOptions "company"

jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
      lowercaseFirstCharacter (x:xs) = toLower x : xs
      lowercaseFirstCharacter []     = []
   in JSON.defaultOptions { JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter
                          }

getUsersContent :: IO LByteString
getUsersContent = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  HTTP.responseBody <$> HTTP.httpLbs request manager

getUsers :: IO (Either String [User])
getUsers = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  (HTTP.responseBody >>> JSON.eitherDecode) <$> HTTP.httpLbs request manager


runMain :: IO ()
runMain = do
  manager <- newTlsManager
  putStrLn "Hello, World!"
