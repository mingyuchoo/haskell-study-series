module Lib
    ( someFunc
    ) where

import           Flow ((<|))

{-
# 문제
어느 누군가가 타임머신을 타고 과거로 가서 숫자 0이 없는 수 체계를 전파했습니다.
역사가 바뀌어 이제 사람들의 의식 속엔 0이란 숫자가 사라졌습니다. 따라서,
현재의 수 체계는 1, 2, 3, ..., 9, 11, ... 와 같이 0이 없게 바뀌었습니다.

0을 포함하지 않는 자연수 num이 매개변수로 주어질 때, 이 수에 1을 더한 수를 return 하도록
solution 메소드를 완성해주세요.

## 매개변수 설명
자연수 num이 solution 메소드의 매개변수로 주어집니다.
* num은 1 이상 999,999,999,999,999,999,999 이하의 0을 포함하지 않는 자연수입니다.

## return 값 설명
자연수 num에 1을 더한 수를 return 해주세요.

## 예시
| num     | return |
| ------- | ------ |
| 9949999 | 9951111|

## 예시 설명
9,949,999에 1을 더하면 9,950,000 이지만 0은 존재하지 안흐므로 9,951,111이 됩니다.
-}

someFunc :: IO ()
someFunc = do
  print <| converter 9949999 1
  return ()

converter :: Int -> Int -> Int
converter x1 x2 =
  read <| concatMap replaceZero . show <| x1 + x2
  where
    replaceZero :: Char -> [Char]
    replaceZero '0' = ['1']
    replaceZero c   = [c]
