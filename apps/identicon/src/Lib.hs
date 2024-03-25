module Lib
    ( someFunc
    ) where

import           Crypto.Hash            (MD5 (..), hash)
import qualified Data.ByteString        as BS
import           Data.ByteString.Base16 (encode)
import           Data.List              (unfoldr)
import           Data.Maybe             (catMaybes)
import           System.IO              (writeFile)


data Image = Image { hex     :: String
                   , color   :: (Int, Int, Int)
                   , grid    :: [[Bool]]
                   , pixlMap :: [[(Int, Int, Int)]]
                   }


someFunc :: String -> IO ()
someFunc input = do
  let hashed = hashInput input
  let colored = pickColor hashed
  let gridBuilt = buildGrid colored
  let filtered = filterOddSquares gridBuilt
  let pixelMapped = buildPixelMap filtered
  image <- drawImage pixelMapped
  saveImage image input


hashInput :: String -> Image
hashInput input = Image { hex = show (hash input :: MD5)
                        , color = (0,0,0)
                        , pixelMap = []
                        , grid = []
                        }
pickColor :: Image -> Image
pickColor image = image { color = (0,0,0) }


buildGrid :: Image -> Image
buildGrid image = image { grid = [] }

filterOddSquares :: Image -> Image
filterOddSquares image = image { grid = filter (even . fst) (grid image) }

buildPixelMap :: Image -> Image
buildPixelMap image = image { pixelMap = [] }

drawImage :: Image -> IO BS.ByteString
drawImage image = do
  return BS.empty


saveImage :: BS.ByteString -> String -> IO ()
saveImage image input = writeFile (input ++ ".png") (BS.unpack image)
