-- | main function

-- type signature, declaring the type of `main`
main :: IO ()

-- function definition: variation 1
main = putStrLn "Hello, World!"

{----------------------------------

-- function definition: variation 2
main = do {
  putStrLn "Hello, World!" ;
  return ()
}


-- function definition: variation 3
main = do
  putStrLn "Hello, World!"
  return ()

----------------------------------}
