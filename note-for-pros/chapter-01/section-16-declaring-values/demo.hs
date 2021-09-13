module Demo where
-- Module names are capitalized, unlike variable names.
-- We declare the name of our module so
-- it can be imported by name in a project.


-- |
x :: Integer
x = 5

-- |
y :: Integer
y = 2 * 5 + x

-- |
result :: Integer
result = y * 10

-- |
main :: IO ()
main = do
  print result
