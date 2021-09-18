module Lib
    where

move :: Char -> (Int,Int) -> (Int,Int)
move 'L' (x,y) = (x-1,y)
move 'R' (x,y) = (x+1,y)
move 'U' (x,y) = (x,y+1)
move 'D' (x,y) = (x,y-1)
move _  _ = error "Error"

someFunc :: IO ()
someFunc = do
  let input = ['U','R','D','D','L']
      output = foldr move (0,0) input
  print output
