{-# LANGUAGE StandaloneKindSignatures #-}
module Lib
    where

import           Control.Lens (element, (&), (.~))
import           Data.Maybe   (mapMaybe)
import           Flow         ((<|))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- -----------------------------------------------------------------------------
-- 1. 가장 가까이 있는 위치 정보를 꺼내기 (deque)
-- 2. 방문 기록하기 (현까지 누적한 걸음 수 + 1)
-- 3. 사방 탐색하기 (동,서,남,북)
-- 4. 범위 밖이면 무시하기
-- 5. 방문할 기록으로 남기기 (enque)
-- 6. 방문할 기록이 없을 때 까지 1 부터 진행하기
-- -----------------------------------------------------------------------------

