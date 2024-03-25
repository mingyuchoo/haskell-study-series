module Lib
    ( someFunc
    ) where

import           Crypto.Hash
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Char8  (pack)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE

-- data Image = Image { hex     :: String
--                    , color   :: (Int, Int, Int)
--                    , grid    :: [[Bool]]
--                    , pixlMap :: [[(Int, Int, Int)]]
--                    }

data Image = Image { hex :: String
                   }

someFunc :: IO ()
someFunc = do
  let input = "abcd"
  let md5Hash = hashMD5 input
  putStrLn $ show $ md5ToHex md5Hash

hashMD5 :: String -> ByteString
hashMD5 input = hash (pack input) :: Digest MD5

md5ToHex :: ByteString -> Text
md5ToHex = TE.decodeUtf8 . B16.encode


-- someFunc :: String -> IO ()
-- someFunc input = do
--   let hashed = hashInput input
--   let colored = pickColor hashed
--   let gridBuilt = buildGrid colored
--   let filtered = filterOddSquares gridBuilt
--   let pixelMapped = buildPixelMap filtered
--   image <- drawImage pixelMapped
--   saveImage image input
--
--
-- hashInput :: String -> Image
-- hashInput input = Image { hex = show (hash input :: MD5)
--                         , color = (0,0,0)
--                         , pixelMap = []
--                         , grid = []
--                         }
-- pickColor :: Image -> Image
-- pickColor image = image { color = (0,0,0) }
--
--
-- buildGrid :: Image -> Image
-- buildGrid image = image { grid = [] }
--
-- filterOddSquares :: Image -> Image
-- filterOddSquares image = image { grid = filter (even . fst) (grid image) }
--
-- buildPixelMap :: Image -> Image
-- buildPixelMap image = image { pixelMap = [] }
--
-- drawImage :: Image -> IO BS.ByteString
-- drawImage image = do
--   return BS.empty
--
--
-- saveImage :: BS.ByteString -> String -> IO ()
-- saveImage image input = writeFile (input ++ ".png") (BS.unpack image)
