module Lib
    where


-- 2s + 10 = s + 15
equation1 :: Maybe Int
equation1 =
  let
    left :: Int -> Int
    left s = 2*s + 10

    right :: Int -> Int
    right s = s + 15

    compare :: Int -> Maybe Int
    compare x
      | x >= 100 = Nothing
      | left x == right x = Just x
      | otherwise = compare (x + 1)
  in
    compare 0
