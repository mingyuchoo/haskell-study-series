module Chapter12.AFistfulOfMonads where
-------------------------------------------------------------------------------
chapter12 :: IO ()
chapter12 = do
  print $ (*)  <$> Just 2         <*> Just 8
  print $ (++) <$> Just "klingon" <*> Nothing
  print $ (-)  <$> [3,4]          <*> [1,2,3]

  print $ fmap (++"!") (Just "wisdom")
  print $ fmap (++"!") Nothing

  print $ Just (+3) <*> Just 3

  print $ max <$> Just 3 <*> Just 6
  print $ max <$> Just 3 <*> Nothing

  print $ (\x -> Just (x+1)) 1
  print $ (\x -> Just (x+1)) 100

  print $ Just 3       `applyMaybe` \x -> Just (x+1)
  print $ Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
  print $ Nothing      `applyMaybe` \x -> Just (x+1)
  print $ Nothing      `applyMaybe` \x -> Just (x ++ " :)")
  print $ Just 3       `applyMaybe` \x -> if x > 2 then Just x else Nothing
  print $ Just 1       `applyMaybe` \x -> if x > 2 then Just x else Nothing

  print $ do return "WHAT" :: Maybe String
  print $ Just 9  >>= \x -> return (x*10)
  print $ Nothing >>= \x -> return (x*10)

  print $ landLeft  2    (0,0)
  print $ landRight 1    (0,0)
  print $ landLeft  (-1) (0,0)
  print $ landLeft 2 (landRight 1 (landLeft 1 (0,0)))

  print $ 100   -: (*3)
  print $ True  -: not
  print $ (0,0) -: landLeft 2
  print $ (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

  print $ landLeft 10 (0,3)
  print $ (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)

  print $ landLeft' 2  (0,0)
  print $ landLeft' 10 (0,3)
  print $ landRight' 1 (0,0) >>= landLeft' 2
  print $ return (0,0) >>= landRight' 2 >>= landLeft' 2  >>= landRight' 2
  print $ return (0,0) >>= landLeft' 1  >>= landRight' 4 >>= landLeft' (-1) >>= landRight' (-2)

  print $ return (0,0) >>= landLeft' 1 >>= banana >>= landRight' 1
  print $ Nothing >> Just 3
  print $ Just 3  >> Just 4
  print $ return (0,0) >>= landLeft' 1 >> Nothing >>= landRight' 1

  print $ Just 3 >>= (\x -> Just (show x ++ "!"))
  print $ Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))


  return ()
-------------------------------------------------------------------------------
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing  f = Nothing
applyMaybe (Just x) f = f x
-------------------------------------------------------------------------------
type Birds = Int
type Pole  = (Birds,Birds)
-------------------------------------------------------------------------------
landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right+1)

-- | (-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x
-------------------------------------------------------------------------------
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)
  | abs (left + n - right) < 4 = Just (left + n,right)
  | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
  | abs (left - (right + n)) < 4 = Just (left, right + n )
  | otherwise                    = Nothing
-------------------------------------------------------------------------------
banana :: Pole -> Maybe Pole
banana _ = Nothing
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------