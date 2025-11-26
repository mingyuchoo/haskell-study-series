{-# LANGUAGE BangPatterns #-}

module FuzzySearch
    ( fuzzyMatch
    , fuzzyScore
    , FuzzyResult(..)
    ) where

import qualified Data.Text as T

-- | 퍼지 매칭 결과
data FuzzyResult = FuzzyResult
    { frScore :: !Int           -- 매칭 점수 (높을수록 좋음)
    , frMatches :: ![Int]       -- 매칭된 인덱스들
    } deriving (Show, Eq)

-- | 퍼지 매칭 알고리즘 (fzf 스타일)
fuzzyMatch :: T.Text -> T.Text -> Maybe FuzzyResult
fuzzyMatch query target
    | T.null query = Just $ FuzzyResult 0 []
    | otherwise = go 0 0 0 []
  where
    queryLower = T.toLower query
    targetLower = T.toLower target
    qLen = T.length query
    tLen = T.length target
    
    go !qIdx !tIdx !score !matches
        | qIdx >= qLen = Just $ FuzzyResult score (reverse matches)
        | tIdx >= tLen = Nothing
        | qChar == tChar = 
            let bonus = calculateBonus tIdx
                newScore = score + bonus
            in go (qIdx + 1) (tIdx + 1) newScore (tIdx : matches)
        | otherwise = go qIdx (tIdx + 1) score matches
      where
        qChar = T.index queryLower qIdx
        tChar = T.index targetLower tIdx

    -- 보너스 점수 계산 (연속 매칭, 단어 시작 등)
    calculateBonus idx
        | idx == 0 = 10  -- 시작 위치
        | isWordBoundary idx = 8  -- 단어 경계
        | otherwise = 1
    
    isWordBoundary idx =
        let prevChar = T.index target (idx - 1)
        in prevChar == '/' || prevChar == '-' || prevChar == '_' || prevChar == ' '

-- | 퍼지 매칭 점수만 반환
fuzzyScore :: T.Text -> T.Text -> Int
fuzzyScore query target = 
    case fuzzyMatch query target of
        Just result -> frScore result
        Nothing -> -1
