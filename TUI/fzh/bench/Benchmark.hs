module Main where

import Criterion.Main
import qualified Data.Text as T
import qualified Data.Vector as V
import FuzzySearch
import ParallelFilter

-- | 테스트 데이터 생성
generateTestData :: Int -> V.Vector T.Text
generateTestData n = V.fromList $ map (\i -> T.pack $ "test_file_" ++ show i ++ ".txt") [1..n]

-- | 벤치마크 메인
main :: IO ()
main = defaultMain
    [ bgroup "fuzzyMatch"
        [ bench "short query" $ nf (fuzzyMatch (T.pack "test")) (T.pack "test_file_123.txt")
        , bench "long query" $ nf (fuzzyMatch (T.pack "testfile")) (T.pack "test_file_123.txt")
        ]
    , bgroup "parallelFilter"
        [ bench "100 items" $ nfIO $ parallelFilter (T.pack "test") (generateTestData 100)
        , bench "1000 items" $ nfIO $ parallelFilter (T.pack "test") (generateTestData 1000)
        , bench "10000 items" $ nfIO $ parallelFilter (T.pack "test") (generateTestData 10000)
        ]
    ]
