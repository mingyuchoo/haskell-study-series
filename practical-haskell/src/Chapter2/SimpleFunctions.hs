{-# LANGUAGE UnicodeSyntax #-}


--------------------------------------------------------------------------------
module Chapter2.SimpleFunctions
    where

--------------------------------------------------------------------------------
-- | firstOrEmpty
--
-- Examples:
--
-- >>> firstOrEmpty []
-- "empty"
--
-- >>> firstOrEmpty ["hello","hola"]
-- "hello"
--
firstOrEmpty ∷ [[Char]] → [Char]
firstOrEmpty list 
  = if not (null list)
    then head list
    else "empty"


--------------------------------------------------------------------------------
-- | (+++)
--
-- Examples:
--
--
(+++) ∷ [a] → [a] → [a]
list1 +++ list2 
  = if null list1 {- check emptyness -}
    then list2    {- base case       -}
    else (head list1) : (tail list1 +++ list2)


-- | (+++!)
--
-- Examples:
--
--
(+++!) ∷ [a] → [a] → [a]
list1 +++! list2 
  = case list1 of
      [] -> list2
      x:xs -> x:(xs +++! list2)


-- | (+++!!)
--
-- Examples:
--
--
(+++!!) ∷ [a] → [a] → [a]
[] +++!! list2 = list2
(x:xs) +++!! list2 = x:(xs +++!! list2)
--------------------------------------------------------------------------------
-- | reverse2
--
-- Examples:
--
--
reverse2 ∷ [a] → [a]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

--------------------------------------------------------------------------------
-- | maxmin
--
-- Examples:
--
--
maxmin ∷ Ord a ⇒ [a] → (a, a)
maxmin list = if null (tail list)
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if (head list) < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list))
                   )


-- | maxmin'
--
-- Examples:
--
--
maxmin' ∷ Ord a ⇒ [a] → (a, a)
maxmin' list 
  = let h = head list
    in if null (tail list)
       then (h, h)
       else ( if h > t_max then h else t_max
            , if h < t_min then h else t_min)
            where
              t = maxmin' (tail list)
              t_max = fst t
              t_min = snd t


-- | maxmin''
--
-- Examples:
--
--
maxmin'' ∷ Ord a ⇒ [a] → (a, a)
maxmin'' [x] = (x, x)
maxmin'' (x:xs) = ( if x > xs_max then x else xs_max
                  , if x < xs_min then x else xs_min
                  )
                  where
                    (xs_max, xs_min) = maxmin'' xs

--------------------------------------------------------------------------------
data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)


data Person
  = Person String String Gender
  deriving (Show)

data Gender = Male | Femail | Unkown deriving (Show)

--------------------------------------------------------------------------------
-- | clientName
--
-- Examples:
--
--
clientName ∷ Client → String
clientName client 
  = case client of
      GovOrg name -> name
      Company name id person resp -> name
      Individual person ads ->
        case person of
          Person fistName lastName gender -> fistName ++ " " ++ lastName


-- | clientName2
--
-- Examples:
--
--
clientName2 ∷ Client → String
clientName2 (GovOrg name) = name
clientName2 (Company name _ _ _) = name
clientName2 (Individual (Person firstName lastName _) _) = firstName ++ " " ++ lastName


-- | clientName'
--
-- Examples:
--
--
clientName' ∷ Client → String
clientName' client 
  = case client of
      GovOrg name -> name
      Company name _ _ _ -> name
      Individual (Person fistName lastName _) _ -> fistName ++ " " ++ lastName


-- | clientName''
--
-- Examples:
--
--
clientName'' ∷ Client → Maybe String
clientName'' client 
  = case client of
      Company name _ _ _ -> Just name
      _ -> Nothing

--------------------------------------------------------------------------------
-- | fibonacci
--
-- Examples:
--
--
fibonacci ∷ Integer → Integer
fibonacci n 
  = case n of
      0 -> 0
      1 -> 1
      _ -> fibonacci (n - 1) + fibonacci (n - 2)


-- | fibonacci'
--
-- Examples:
--
--
fibonacci' ∷ Integer → Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n = fibonacci (n - 1) + fibonacci (n - 2)


-- | ifibonacci
--
-- Example:
--
--
ifibonacci ∷ Integer → Maybe Integer
ifibonacci n =
  if n < 0
  then Nothing
  else case n of
    0 -> Just 0
    1 -> Just 1
    n' -> let Just f1 = ifibonacci (n' - 1)
              Just f2 = ifibonacci (n' - 2)
          in Just (f1 + f2)

-- | ifibonacci'
--
-- Example:
--
--
ifibonacci' ∷ Integer → Maybe Integer
ifibonacci' n | n < 0  = Nothing
ifibonacci' 0 = Just 0
ifibonacci' 1 = Just 1
ifibonacci' n | others = let Just f1 = ifibonacci' (n - 1)
                             Just f2 = ifibonacci' (n - 2)
                         in Just (f1 + f2)
--------------------------------------------------------------------------------
-- | f
--
-- Examples:
--
--
f ∷ Client → String
f client 
  = case client of
      Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
      _ -> "There is no boss"

-- | g
--
-- Examples:
--
--
g ∷ Client → String
g client 
  = case client of
      Company _ _ (Person name _ _) pos ->
        case pos of
          "Boss" -> name ++ " is the boss"
      _ -> "There is no boss"

--------------------------------------------------------------------------------
-- | sorted
--
-- Examples:
--
--
sorted ∷ [Integer] → Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = x < y && sorted (y:zs)


-- | sorted'
--
-- Examples:
--
--
sorted' ∷ [Integer] → Bool
sorted' [] = True
sorted' [_] = True
sorted' (x:r@(y:_)) = x < y && sorted' r


-- | sorted''
--
-- Examples:
--
--
sorted'' ∷ [Integer] → Bool
sorted'' list 
  = case list of
      [] -> True
      [_] -> True
      (x:r@(y:_)) -> x < y && sorted'' r

--------------------------------------------------------------------------------
-- | binom
--
-- Examples:
--
--
binom _ 0 = 1
binom x x = 1
binom n k = (binom (n - 1) (k - 1)) + (binom (n - 1) k)


-- | binom'
--
-- Examples:
--
--
binom' _ 0 = 1
binom' x y | x == y = 1
binom' n k = (binom (n - 1) (k - 1)) + (binom (n - 1) k)


--------------------------------------------------------------------------------
-- | multipleOf
--
-- Examples:
--
--
multipleOf ∷ Integer → Integer → Bool
multifplOf x y = (mod x y) == 0


