module Fuzzy
    ( filterItems
    , fuzzyMatchScore
    ) where

import           Data.List   (sortOn)
import qualified Data.Text   as T
import qualified Data.Vector as Vec

-- | 퍼지 매칭 점수 계산 함수 (Pure)
-- 쿼리와 텍스트를 비교하여 매칭 점수 반환
-- Nothing: 매칭 안됨, Just score: 매칭됨 (점수가 낮을수록 좋은 매칭)
fuzzyMatchScore :: T.Text -> T.Text -> Maybe Int
fuzzyMatchScore query text = go (T.toLower query) (T.toLower text) 0
  where
    -- | 재귀적으로 문자를 비교하며 갭(건너뛴 문자 수) 계산 (Pure)
    go q t gaps
      | T.null q  = Just gaps   -- 모든 쿼리 문자 매칭 완료
      | T.null t  = Nothing     -- 텍스트 끝났는데 쿼리 남음
      | T.head q == T.head t = go (T.tail q) (T.tail t) gaps
      | otherwise = go q (T.tail t) (gaps + 1)

-- | 파일 경로의 깊이(슬래시 개수) 계산 (Pure)
-- 정렬 시 얕은 경로 우선을 위해 사용
pathDepth :: T.Text -> Int
pathDepth t = T.count "/" t + T.count "\\" t

-- | 검색어로 아이템 필터링 및 점수순 정렬 (Pure)
-- 빈 쿼리면 전체 반환, 아니면 매칭되는 항목만 점수순 정렬
filterItems :: T.Text -> Vec.Vector T.Text -> Vec.Vector T.Text
filterItems query items
  | T.null query = items
  | otherwise    = Vec.fromList . map fst3 . sortOn snd3 $ scored
  where
    scored = [ (item, (score, pathDepth item), ())
             | item <- Vec.toList items
             , Just score <- [fuzzyMatchScore query item]
             ]
    -- | 3-튜플에서 첫 번째 요소 추출 (Pure)
    fst3 (a, _, _) = a
    -- | 3-튜플에서 두 번째 요소 추출 (Pure)
    snd3 (_, b, _) = b
