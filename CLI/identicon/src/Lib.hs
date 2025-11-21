module Lib
    ( someFunc
    ) where

import           Crypto.Hash            (Digest, MD5, hash)

import           Data.ByteArray         (convert)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as BS
import           Data.Kind              (Type)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE

-- data Image = Image { hex     :: String
--                    , color   :: (Int, Int, Int)
--                    , grid    :: [[Bool]]
--                    , pixlMap :: [[(Int, Int, Int)]]
--                    }

type Image :: Type
data Image = Image { hex :: String
                   }

-- |
--
someFunc :: IO ()
someFunc = do
  let input = "abcd"
  let md5Hash = hashMD5 input
  putStrLn $ show $ md5ToHex md5Hash

-- |
--
hashMD5 :: String -> ByteString
hashMD5 input = convert (hash (BS.pack input) :: Digest MD5)

-- |
--
md5ToHex :: ByteString -> Text
md5ToHex = TE.decodeUtf8 . B16.encode
