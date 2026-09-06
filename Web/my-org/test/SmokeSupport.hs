module SmokeSupport
  ( Client, withFreshServer, withClient, request, call, get, post, field, items, number
  , first, empty, object, (.=), Value(..), shouldHave, goal, authority
  ) where

import Control.Exception (bracket)
import Data.Aeson (Value(..), object, (.=), encode, eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as BS
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import MyOrg.Server (application)
import MyOrg.Store (openFileStore, closeStore)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Handler.Warp (testWithApplication)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

type Client = (Manager, Int)

withClient :: Int -> (Client -> IO a) -> IO a
withClient port action = newManager defaultManagerSettings >>= \manager -> action (manager, port)

withFreshServer :: (Client -> IO ()) -> IO ()
withFreshServer action = withSystemTempDirectory "my-org-http-" $ \directory ->
  bracket (openFileStore (directory </> "events.json")) closeStore $ \store ->
    testWithApplication (pure (application store)) $ \port -> withClient port action

request :: Client -> String -> String -> Maybe Value -> IO (Int, Value)
request (manager, port) verb path body = do
  initial <- parseRequest ("http://127.0.0.1:" <> show port <> "/api/" <> path)
  response <- httpLbs initial
    { method = BS.pack verb
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody = maybe (RequestBodyLBS "") (RequestBodyLBS . encode) body
    , responseTimeout = responseTimeoutMicro 5000000
    } manager
  payload <- either (fail . ((verb <> " " <> path <> ": invalid JSON: ") <>)) pure (eitherDecode (responseBody response))
  pure (statusCode (responseStatus response), payload)

call :: Client -> String -> String -> Maybe Value -> Int -> IO Value
call client verb path body expected = do
  (status, payload) <- request client verb path body
  if status == expected then pure payload else do
    expectationFailure (verb <> " " <> path <> ": expected " <> show expected <> ", got " <> show status <> ": " <> show payload)
    pure payload

get :: Client -> String -> IO Value
get client path = call client "GET" path Nothing 200

post :: Client -> String -> Value -> Int -> IO Value
post client path body = call client "POST" path (Just body)

field :: Value -> Text -> Value
field (Object fields) key = maybe (error ("Missing JSON field: " <> show key)) id (KM.lookup (Key.fromText key) fields)
field value key = error ("Expected object for " <> show key <> ": " <> show value)

items :: Value -> [Value]
items (Array values) = F.toList values
items value = error ("Expected array: " <> show value)

number :: Value -> Int
number (Number value) = maybe (error "Expected integer") id (toBoundedInteger value)
number value = error ("Expected number: " <> show value)

empty :: Value
empty = object []

shouldHave :: (Eq a, Show a) => [a] -> [a] -> Expectation
shouldHave actual expected = mapM_ (\value -> actual `shouldContain` [value]) expected

goal :: Text -> Text -> Value
goal identifier organization = object
  [ "id" .= identifier, "organization" .= organization, "description" .= ("매출 성장" :: Text)
  , "metric" .= object ["id" .= ("sales" :: Text), "name" .= ("매출" :: Text), "unit" .= ("KRW" :: Text), "direction" .= ("HigherIsBetter" :: Text)]
  , "baseline" .= (0 :: Int), "target" .= (100 :: Int), "startsAt" .= ("2026-01-01T00:00:00Z" :: Text)
  , "deadline" .= ("2027-01-01T00:00:00Z" :: Text), "requiredPermissions" .= ([] :: [Text]), "requiredBudget" .= (0 :: Int)]

authority :: Text -> Int -> Bool -> Value
authority owner budget pricing = object ["owner" .= owner, "budgetLimit" .= budget, "canHire" .= False, "canChangePrice" .= pricing, "canApprove" .= ([] :: [Text])]

first :: [a] -> a
first (value : _) = value
first [] = error "Expected a nonempty JSON array"
