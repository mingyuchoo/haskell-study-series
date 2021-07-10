module Chapter13.ForAFewMonadsMore where
-------------------------------------------------------------------------------
import           Data.Monoid
-------------------------------------------------------------------------------
chapter13 :: IO ()
chapter13 = do
  print $ Sum 3 `mappend` Sum 9
  print $ ("beans",   Sum 10) `applyLog'` addDrink
  print $ ("jerky",   Sum 25) `applyLog'` addDrink
  print $ ("dogmeat", Sum 5)  `applyLog'` addDrink
  print $ ("dogmeat", Sum 5)  `applyLog'` addDrink `applyLog'` addDrink

-------------------------------------------------------------------------------
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f =
  let (y,newLog) = f x
  in  (y,log ++ newLog)
-------------------------------------------------------------------------------
applyLog' :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog' (x,log) f =
  let (y,newLog) = f x
  in  (y,log `mappend` newLog)
-------------------------------------------------------------------------------
type Food  = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk",    Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer",    Sum 30)
-------------------------------------------------------------------------------
