module Lib
    where

import           Text.Printf (printf)

-- -----------------------------------------------------------------------------
-- Lib.hs
-- $ stack run
-- (-0.000000, -0.707107)
-- (-0.707107,  0.000000)
-- ( 0.000000,  0.707107)
-- ( 0.707107, -0.000000)
-- -----------------------------------------------------------------------------

-- | 실행 함수
--
--
someFunc :: IO ()
someFunc = do
  -- (0.5,0.5)를 중심으로 반시계 방향으로 45도 회전시킨 다음 (-0.5,-0.5 병행 이동시키는 설정)
  let config = Config { rotAt = (0.5, 0.5)
                      , theta = pi / 4
                      , ofs   = (-0.5, -0.5)
                      }
  -- 변환 전의 좌표, 예를 들면 이 네 개 점으로 이루어진 정사각형
  let unitRect = [ (0, 0)
                 , (0, 1)
                 , (1, 1)
                 , (1, 0)
                 ]
  -- 변한 후의 좌표
  let convertedRect = map (convertByConfig config) unitRect
  mapM_ (uncurry (printf "(%.6f,%.6f)\n")) convertedRect


-- | 좌표 타입
--
--
type Coordinate = (Double, Double)

-- | 각도 타입
--
--
type Radian = Double

-- | 좌표 변환 설정
--
--
data Config = Config { rotAt :: Coordinate
                       -- ^ 회전 중심 좌표
                     , theta :: Radian
                       -- ^ 회전량[라디안]
                     , ofs   :: Coordinate
                       -- ^ 병행 이동량
                     }


-- | 병행 이동 함수
--
--
trans :: Coordinate -- ^ 변동 값
      -> Coordinate -- ^ 이전 위치
      -> Coordinate -- ^ 이후 위치
trans (dx, dy) (x, y) =
  (x + dx, y + dy)


-- | 회전 함수
--
--
rotate :: Radian     -- ^ 각도
       -> Coordinate -- ^ 이전 위치
       -> Coordinate -- ^ 이후 위치
rotate t (x,y) =
  (cos t * x - sin t * y, sin t * x + cos t * y)


-- | 설정을 바탕으로 한 병행 이동
--
--
transByConfig :: Config     -- ^ 좌표 설정 값
              -> Coordinate -- ^ 이전 위치
              -> Coordinate -- ^ 이후 위치
transByConfig config =
  trans (ofs config)


-- | 설정을 바탕으로 한 회전
--
--
rotateByConfig :: Config     -- ^ 좌표 설정 값
               -> Coordinate -- ^ 이전 위치
               -> Coordinate -- ^ 이후 위치
rotateByConfig config =
  postTrans . rotate (theta config) . preTrans
    where
      rotateAt  = rotAt config
      preTrans  = trans (rotate pi rotateAt)
      postTrans = trans rotateAt


-- | 설정을 바탕으로 한 좌표 변환
--
--
convertByConfig :: Config     -- ^ 좌표 설정 값
                -> Coordinate -- ^ 이전 위치
                -> Coordinate -- ^ 이후 위치
convertByConfig config =
  transByConfig config . rotateByConfig config

