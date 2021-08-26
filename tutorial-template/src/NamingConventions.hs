--------------------------------------------------------------------------------
module NamingConventions where

--------------------------------------------------------------------------------

data MyTypeConstructor = MyDataConstructor String


-- :kind MyTypeClass :: * -> Constraint
-- :kind MyTypeClass Int :: Constraint
-- :kind MyTypeClass MyTypeConstructor :: Constraint
-- :type name :: MyTypeClass a => a -> String
class MyTypeClass a where
  name :: a -> String



-- :type name :: MyTypeClass a => a -> String
-- :type name (MyDataConstructor "a") :: String
instance MyTypeClass MyTypeConstructor where
  name (MyDataConstructor name) = name


--------------------------------------------------------------------------------
func :: p -> p
func x  = x

func' :: p -> p
func' y = y


--------------------------------------------------------------------------------
solution :: IO ()
solution = do
  print $ name (MyDataConstructor "Haskell")
  print $ 100 + 200
  print $ (+) 100 200
