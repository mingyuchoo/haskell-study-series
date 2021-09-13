{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString, pack)
import           Data.Text       (Text, pack)

withString :: String
withString = "Hello String"

-- The following two examples are only allowed with OverloadedStrings.

withText :: Text
withText = "Hello Text" -- instead of: withText = Data.Text.pack "Hello Text"

withBS :: ByteString
withBS = "Hello ByteString" -- instead of: withBS = Data.ByteString.pack "Hello ByteString"


main :: IO ()
main = do
    print withString
    print withText
    print withBS
    return ()

