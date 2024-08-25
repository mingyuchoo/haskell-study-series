module Lib
    ( appRunner
    ) where

import           Flow                           ((<|))

import           Statistics.Distribution
import           Statistics.Distribution.Normal


-- 블랙-숄즈 공식의 변수들을 계산하는 함수
d1 :: (Floating a)
   => a -- ^ s: 자산의 현재 가격
   -> a -- ^ k: 옵션의 행사 가격
   -> a -- ^ r: 무위험 이자율
   -> a -- ^ t: 만기까지의 시간
   -> a -- ^ sigma: 자산 가격의 변동성
   -> a -- ^ Result: 결과값
d1 s k r t sigma = (log (s / k) + (r + sigma^2 / 2) * t) / (sigma * sqrt t)


d2 :: (Floating a)
   => a -- ^ s: 자산의 현재 가격
   -> a -- ^ k: 옵션의 행사 가격
   -> a -- ^ r: 무위험 이자율
   -> a -- ^ t: 만기까지의 시간
   -> a -- ^ sigma: 자산 가격의 변동성
   -> a -- ^ Result: 결과값
d2 s k r t sigma = d1 s k r t sigma - sigma * sqrt t


-- 표준 정규 분포 누적 분포 함수(CDF)를 계산하는 함수
cumulativeNormal :: Double -> Double
cumulativeNormal = cumulative (standard :: NormalDistribution)


-- 콜 옵션 가격을 계산하는 함수
blackScholesCall :: Double -- ^ s: 자산의 현재 가격
                 -> Double -- ^ k: 옵션의 행사 가격
                 -> Double -- ^ r: 무위험 이자율
                 -> Double -- ^ t: 만기까지의 시간
                 -> Double -- ^ sigma: 자산 가격의 변동성
                 -> Double -- ^ Result: 결과값
blackScholesCall s k r t sigma =
  s * cumulativeNormal (d1 s k r t sigma) - k * exp (-r * t) * cumulativeNormal (d2 s k r t sigma)


-- 풋 옵션 가격을 계산하는 함수
blackScholesPut :: Double -- ^ s: 자산의 현재 가격
                -> Double -- ^ k: 옵션의 행사 가격
                -> Double -- ^ r: 무위험 이자율
                -> Double -- ^ t: 만기까지의 시간
                -> Double -- ^ sigma: 자산 가격의 변동성
                -> Double -- ^ Result: 결과값
blackScholesPut s k r t sigma =
  k * exp (-r * t) * cumulativeNormal (-d2 s k r t sigma) - s * cumulativeNormal (-d1 s k r t sigma)


-- 예제 사용
appRunner :: IO ()
appRunner = do
  let s     = 100.0  -- 자산의 현재 가격(S0)
  let k     = 100.0  -- 옵션의 행사 가격(X)
  let r     =   0.05 -- 무위험 이자율(r)
  let t     =   1.0  -- 만기까지의 시간(t,년)
  let sigma =   0.2  -- 자산 가격의 변동성(σ)

  putStrLn <| "Call Option Price: " <> show (blackScholesCall s k r t sigma)
  putStrLn <| "Put Option Price: " <> show (blackScholesPut s k r t sigma)
