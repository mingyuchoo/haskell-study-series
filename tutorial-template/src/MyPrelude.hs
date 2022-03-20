module MyPrelude
    where

--------------------------------------------------------------------------------
import           Data.List ()
import           Flow      ()

--------------------------------------------------------------------------------
length' :: [a] -> Int
length' list = case list of
  []     -> 0
  (x:xs) ->  1 + length' xs


--------------------------------------------------------------------------------
take' :: Int ->  [a] -> [a]
take' num list =
  if num == 0 then [] else
    case list of
      []     -> []
      (x:xs) -> x : take' (num - 1) xs


--------------------------------------------------------------------------------
drop' :: Int -> [a] -> [a]
drop' num list =
  if num == 0 then list else
    case list of
      []   -> []
      x:xs -> drop' (num - 1) xs

--------------------------------------------------------------------------------
insert' :: Ord a => a -> [a] -> [a]
insert' item list =
  case list of
    [] -> [item]
    x:xs -> if item <= x
            then item : x : xs
            else x : insert' item xs


isort :: Ord a => [a] -> [a]
isort list =
  case list of
    []   -> []
    x:xs -> insert' x (isort xs)


--------------------------------------------------------------------------------
merge' :: Ord a => [a] -> [a] -> [a]
merge' first second =
  case (first, second) of
    ([], second) -> second
    (first, []) -> first
    (x:xs, y:ys) ->
      if x < y
      then x : merge' xs (y:ys)
      else y : merge' (x:xs) ys


msort :: Ord a => [a] -> [a]
msort ls =
  case ls of
    []  -> []
    [l] -> [l]
    ls  ->
      let left  = take' (length' ls `div` 2) ls
          right = drop' (length' ls `div` 2) ls
      in merge' (msort left) (msort right)
