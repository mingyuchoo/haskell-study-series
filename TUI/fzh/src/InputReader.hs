module InputReader
    ( readInputLines
    , streamInputLines
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.IO (stdin, hIsEOF)
import qualified Streaming.Prelude as S
import Streaming (Stream, Of)

-- | 표준 입력에서 모든 라인 읽기
readInputLines :: IO (V.Vector T.Text)
readInputLines = do
    contents <- TIO.getContents
    return $ V.fromList $ T.lines contents

-- | 스트리밍 방식으로 입력 읽기 (메모리 효율적)
streamInputLines :: IO (Stream (Of T.Text) IO ())
streamInputLines = return $ S.untilRight go
  where
    go = do
        eof <- hIsEOF stdin
        if eof
            then return $ Right ()
            else do
                line <- TIO.hGetLine stdin
                return $ Left line
