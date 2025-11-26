{-# LANGUAGE BangPatterns #-}

module ParallelFilter
    ( parallelFilter
    , FilterResult(..)
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (getNumCapabilities)
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import FuzzySearch

-- | 필터링 결과
data FilterResult = FilterResult
    { filterText :: !T.Text
    , filterScore :: !Int
    , filterMatches :: ![Int]
    } deriving (Show, Eq)

-- | 병렬 퍼지 필터링
parallelFilter :: T.Text -> V.Vector T.Text -> IO [FilterResult]
parallelFilter query items
    | T.null query = return $ map (\t -> FilterResult t 0 []) (V.toList items)
    | otherwise = do
        numCaps <- getNumCapabilities
        let chunks = splitVector numCaps items
        results <- mapConcurrently (filterChunk query) chunks
        let allResults = concat results
            sorted = sortBy (comparing (Down . filterScore)) allResults
        return sorted

-- | 벡터를 청크로 분할
splitVector :: Int -> V.Vector a -> [V.Vector a]
splitVector n vec
    | n <= 0 = [vec]
    | V.null vec = []
    | otherwise = 
        let len = V.length vec
            chunkSize = max 1 ((len + n - 1) `div` n)
        in go 0 chunkSize
  where
    go !start !size
        | start >= V.length vec = []
        | otherwise = 
            let chunk = V.slice start (min size (V.length vec - start)) vec
            in chunk : go (start + size) size

-- | 청크 필터링
filterChunk :: T.Text -> V.Vector T.Text -> IO [FilterResult]
filterChunk query chunk = return $ V.foldr' go [] chunk
  where
    go item acc = 
        case fuzzyMatch query item of
            Just result -> FilterResult item (frScore result) (frMatches result) : acc
            Nothing -> acc
