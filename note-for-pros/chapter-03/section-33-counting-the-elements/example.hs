main :: IO ()
main = do
    print $ length [7,2,9]
    print $ length (Right 'a')
    print $ length (Left "foo")
    print $ length (3, True)

