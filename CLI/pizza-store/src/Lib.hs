module Lib
    where

-- |
--
type Pizza = [Char]
type Price = Int
type Menu  = (Pizza,Price)

-- |
--
solution :: [Menu] -> [Pizza] -> Price -> Price
solution [] [] sum = sum + 0
solution _  [] sum = sum + 0
solution menu (x:xs) sum =
    if x == fst (head menu)
    then solution menu xs (sum + (snd (head menu)))
    else solution (tail menu) (x:xs) sum

-- |
--
someFunc :: IO ()
someFunc = do
    let menu  = [ ("Cheese",11100)
                , ("Potato",12600)
                , ("Shrimp",13300)
                , ("Pineapple",21000)
                , ("Meatball",19500)
                ]
        order = ["Cheese","Pineapple","Meatball"]
    print $ solution menu order 0
