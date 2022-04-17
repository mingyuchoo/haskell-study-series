module Lib
    where

import           Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- -----------------------------------------------------------------------------

newtype Stack a = Stack [a]
                deriving (Show)

emptyS :: Stack a
emptyS = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

-- -----------------------------------------------------------------------------

newtype Queue a = Queue [a]
                deriving (Show)


emptyQ :: Queue a
emptyQ = Queue []


enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs <> [x])


dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue [])     = (Nothing, Queue [])
dequeue (Queue (x:xs)) = (Just x, Queue xs)

-- -----------------------------------------------------------------------------
