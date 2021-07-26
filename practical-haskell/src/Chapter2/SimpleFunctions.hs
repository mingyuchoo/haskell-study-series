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
firstOrEmpty list = if not (null list) then head list else "empty"


--------------------------------------------------------------------------------
(+++) ∷ [a] → [a] → [a]
list1 +++ list2 = if null list1 {- check emptyness -}
                then list2    {- base case       -}
                else (head list1) : (tail list1 +++ list2)

--------------------------------------------------------------------------------
reverse2 ∷ [a] → [a]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

--------------------------------------------------------------------------------
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


maxmin' ∷ Ord a ⇒ [a] → (a, a)
maxmin' list =
  let h = head list
  in if null (tail list)
     then (h, h)
     else ( if h > t_max then h else t_max
          , if h < t_min then h else t_min)
          where
            t = maxmin' (tail list)
            t_max = fst t
            t_min = snd t
--------------------------------------------------------------------------------
data Client
  = GovOrg String
  | Company String Integer String String
  | Individual Person Bool
  deriving (Show)


data Person
  = Person String String Gender
  deriving (Show)

data Gender = Male | Femail | Unkown deriving (Show)

--------------------------------------------------------------------------------
clientName ∷ Client → String
clientName client 
  = case client of
      GovOrg name -> name
      Company name id person resp -> name
      Individual person ads ->
        case person of
          Person fistName lastName gender -> fistName ++ " " ++ lastName


clientName' ∷ Client → String
clientName' client 
  = case client of
      GovOrg name -> name
      Company name _ _ _ -> name
      Individual (Person fistName lastName _) _ -> fistName ++ " " ++ lastName


clientName'' ∷ Client → Maybe String
clientName'' client 
  = case client of
      Company name _ _ _ -> Just name
      _ -> Nothing

--------------------------------------------------------------------------------
fibonacci ∷ Integer → Integer
fibonacci n 
  = case n of
      0 -> 0
      1 -> 1
      _ -> fibonacci (n - 1) + fibonacci (n - 2)
