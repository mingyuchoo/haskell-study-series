-- | Small JSON combinators with no application or presentation dependencies.
module MyOrg.Serialization.Codec
  ( Codec (..)
  , textCodec
  , boolCodec
  , intCodec
  , integerCodec
  , doubleCodec
  , timeCodec
  , valueCodec
  , listCodec
  , maybeCodec
  , setCodec
  , mapCodec
  , field
  , optionalField
  , record
  , tagged
  , encodeBytes
  , decodeBytes
  ) where

import Data.Aeson (Key, Object, Value (..), object, withArray)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as F
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)

data Codec a = Codec
  { encodeValue :: a -> Value
  , decodeValue :: Value -> Parser a
  }

textCodec :: Codec Text
textCodec = Codec A.toJSON A.parseJSON
boolCodec :: Codec Bool
boolCodec = Codec A.toJSON A.parseJSON
intCodec :: Codec Int
intCodec = Codec A.toJSON A.parseJSON
integerCodec :: Codec Integer
integerCodec = Codec A.toJSON A.parseJSON
doubleCodec :: Codec Double
doubleCodec = Codec A.toJSON A.parseJSON
timeCodec :: Codec UTCTime
timeCodec = Codec A.toJSON A.parseJSON
valueCodec :: Codec Value
valueCodec = Codec id pure

listCodec :: Codec a -> Codec [a]
listCodec codec =
  Codec
    (A.toJSON . map (encodeValue codec))
    (withArray "list" (mapM (decodeValue codec) . F.toList))

maybeCodec :: Codec a -> Codec (Maybe a)
maybeCodec codec = Codec (maybe Null (encodeValue codec)) decode
  where
    decode Null  = pure Nothing
    decode value = Just <$> decodeValue codec value

setCodec :: (Ord a) => Codec a -> Codec (Set a)
setCodec codec = mapCodec Set.fromList Set.toList (listCodec codec)

mapCodec :: (a -> b) -> (b -> a) -> Codec a -> Codec b
mapCodec wrap unwrap codec = Codec (encodeValue codec . unwrap) (fmap wrap . decodeValue codec)

field :: Codec a -> Object -> Key -> Parser a
field codec obj key = (obj A..: key) >>= decodeValue codec

optionalField :: Codec a -> Object -> Key -> Parser (Maybe a)
optionalField codec obj key = case KM.lookup key obj of
  Nothing    -> pure Nothing
  Just Null  -> pure Nothing
  Just value -> Just <$> decodeValue codec value

record :: [(Key, Maybe Value)] -> Value
record = object . catMaybes . map (\(key, value) -> fmap (key,) value)

tagged :: Text -> Maybe Value -> Value
tagged tag contents = record [("tag", Just (String tag)), ("contents", contents)]

encodeBytes :: Codec a -> a -> BL.ByteString
encodeBytes codec = A.encode . encodeValue codec

decodeBytes :: Codec a -> BL.ByteString -> Either String a
decodeBytes codec bytes = A.eitherDecode bytes >>= parseEither (decodeValue codec)
