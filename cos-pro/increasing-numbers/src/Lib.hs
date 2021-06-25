module Lib
    ( someFunc
    ) where

-- ------------------------------------------------------------------------- --

data MyTypeConstructor = MyDataConstructor deriving (Show)


class MyTypeClass myTypeVariable where
  myTypeClassFunction :: myTypeVariable -> String


instance MyTypeClass MyTypeConstructor where
  myTypeClassFunction MyDataConstructor = "MyValue"


myOtherFunction :: String -> MyTypeConstructor
myOtherFunction "MyValue" = MyDataConstructor
-- ------------------------------------------------------------------------- --

someFunc :: IO ()
someFunc = print "Hello, World"
