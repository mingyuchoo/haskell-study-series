{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
module NamingConventions where

--------------------------------------------------------------------------------
newtype Dollars = Dollars Int

instance (Num Int) => Num Dollars where        -- `Num Int =>` needs `FlexibleContexts` pragma
  Dollars a + Dollars b = Dollars (a + b)
  -- this needs `UndecidableInstances` pragma
  --   because we didn't implement all of functions

--------------------------------------------------------------------------------

data MyTypeConstructor a = MyDataConstructor a


-- :kind MyTypeClass :: * -> Constraint
-- :kind MyTypeClass Int :: Constraint
-- :kind MyTypeClass MyTypeConstructor :: Constraint
-- :type name :: MyTypeClass a => a -> String
class MyTypeClass b where
  name :: b -> String


-- :type name :: MyTypeClass a => a -> String
-- :type name (MyDataConstructor "a") :: String
instance (MyTypeClass a) => MyTypeClass (MyTypeConstructor a) where
  name (MyDataConstructor x) = "Instance of MyClassType"


--------------------------------------------------------------------------------
func :: p -> p
func x  = x

func' :: p -> p
func' y = y


--------------------------------------------------------------------------------
solution :: IO ()
solution = do
--  print $ name (MyDataConstructor "Haskell")
  print $ 100 + 200
  print $ (+) 100 200

