module Lib
    where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Given a list of `N` numbers
-- and with the following constrains
--  - can add numbers `M` times
--  - can use same item `K` times in a row
-- When generate a sum value with the list
-- and the constrains
-- Then I should get the biggest sum value.
