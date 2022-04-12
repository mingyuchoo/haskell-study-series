module Lib
    where

someFunc :: IO ()
someFunc =
  let matrix = (4,4)
      coordiante = (1,1,0)
      map = [[1,1,1,1],[1,0,0,1],[1,1,0,1],[1,1,1,1]]
  in
    putStrLn "Hello, World"
