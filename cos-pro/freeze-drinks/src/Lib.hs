module Lib
    where


someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- -----------------------------------------------------------------------------

newtype Stack a =
  Stack [a] deriving (Show)

emptyS :: Stack a
emptyS = Stack []

-- |
--
-- >>> emptyS `push` 1 `push` 2 `push` 3
-- Stack [3,2,1]
--
push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

-- |
--
-- >>> pop $ Stack[3,2,1]
-- (Just 3,Stack [2,1])
--
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

-- -----------------------------------------------------------------------------

newtype Queue a =
  Queue [a] deriving (Show)


emptyQ :: Queue a
emptyQ = Queue []


-- |
--
-- >>> emptyQ `enque` 1 `enque` 2 `enque` 3
-- Stack [1,2,3]
--
enque :: Queue a -> a -> Queue a
enque (Queue xs) x = Queue (xs <> [x])

-- |
--
-- >>> deque $ Queue[1,2,3]
-- (Just 1,Queue [2,3])
--
deque :: Queue a -> (Maybe a, Queue a)
deque (Queue [])     = (Nothing, Queue [])
deque (Queue (x:xs)) = (Just x, Queue xs)

-- -----------------------------------------------------------------------------
