module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- I want to get the largest number
-- from the result of
-- picking the smallest number in each row.

-- M: the number of columns
-- N: the number of rows
-- N x M: matrix of numbers

n1 = 3
m1 = 3
matrix1 = [[3,1,2],[4,1,4],[2,2,2]]
anwer1 = 2


n2 = 2
m2 = 4
matrix2 = [[27,3,1,8],[3,3,3,4]]
anwer2 = 3
