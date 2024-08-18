module Lib
    ( someFunc
    ) where

import           Flow ((<|))

someFunc :: IO ()
someFunc = do
  let
    count = 5
  putStr <| triangle1 count
  putStr <| triangle2 count
  putStr <| triangle2' count
  putStr <| triangle3 count
  putStr <| upsideDownTriangle1 count
  putStr <| upsideDownTriangle2 count
  putStr <| upsideDownTriangle3 count


{- Q: Draw a triangle (n = 5)
 ----
 *
 **
 ***
 ****
 *****
 -}

-- NOT GOOD
triangle1 :: Int -> String
triangle1 0 = ""
triangle1 n = triangle1 (n - 1) ++ replicate n '*' ++ "\n"

-- NOT GOOD
triangle2 :: Int -> String
triangle2 n =
  case n of
    0 -> ""
    _ -> triangle1 (n - 1) ++ replicate n '*' ++ "\n"

triangle2' :: Int -> String
triangle2' n =
  case () of
    _ | n > 0     -> triangle1 (n - 1) ++ replicate n '*' ++ "\n"
      | otherwise -> ""


triangle3 :: Int -> String
triangle3 n
  | n > 0 = triangle3 (n - 1) ++ replicate n '*' ++ "\n"
  | otherwise = ""


{- Q: Draw a upside down triangle (n = 5)
 ----
 *****
 ****
 ***
 **
 *
 -}

upsideDownTriangle1 :: Int -> String
upsideDownTriangle1 0 = ""
upsideDownTriangle1 n = replicate n '*' ++ "\n" ++ upsideDownTriangle1 (n-1)


upsideDownTriangle2 :: Int -> String
upsideDownTriangle2 n =
  case n of
    0 -> ""
    _ -> replicate n '*' ++ "\n" ++ upsideDownTriangle2 (n-1)


upsideDownTriangle3 :: Int -> String
upsideDownTriangle3 n
  | n > 0 = replicate n '*' ++ "\n" ++ upsideDownTriangle3 (n-1)
  | otherwise = ""
