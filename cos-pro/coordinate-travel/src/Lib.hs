module Lib
    where

-- |
--
--
move :: Int        -- ^ 최대 공간 크기
     -> (Int, Int) -- ^ 좌표 값
     -> String     -- ^ 이동 명령
     -> (Int, Int) -- ^ 이경 후 좌표 값
move n (x, y) c | c == "L" = bound (x, y - 1) n
                | c == "R" = bound (x, y + 1) n
                | c == "U" = bound (x - 1, y) n
                | c == "D" = bound (x + 1, y) n
                | otherwise = error "error: coordinate-travel is incorrect."

-- |
--
--
bound :: (Int, Int) -- ^ 좌표 값
      -> Int        -- ^ 최대 공간 크기
      -> (Int, Int) -- ^ 경계 처리한 좌표 값
bound (x, y) b | x < 1     = (1, y)
               | x > b     = (b, y)
               | y < 1     = (x, 1)
               | y > b     = (x, b)
               | otherwise = (x, y)
