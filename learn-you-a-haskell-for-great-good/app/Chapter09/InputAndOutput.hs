-- module Chapter09.InputAndOutput
-- (
-- 
-- ) where

-- main = putStrLn "Hello, World!"

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

