-- {-# OPTIONS_GHC -F -pgmF doctest-discover #-}
-- {-# OPTIONS_GHC -F -pgmF hspec-discover   #-}

import Test.Hspec
import Lib
import LSP.Types
import Data.Aeson (encode, decode, Value(..))
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Given Prelude" $ do
        context "when use `read` function" $ do
            it "should parse integers" $ do
                read "10" `shouldBe` (10 :: Int)
            it "should parse floating-point numbers" $ do
                read "2.5" `shouldBe` (2.5 :: Float)
    describe "Given Lib" $ do
        context "when use `cliMain` function" $ do
            it "should be succeeded" $ do
              cliMain
    describe "Given LSP.Types" $ do
        context "when serializing LspMessage" $ do
            it "should encode and decode RequestMessage correctly" $ do
                let msg = RequestMessage (String "1") "initialize" Null
                decode (encode msg) `shouldBe` Just msg
            it "should encode and decode NotificationMessage correctly" $ do
                let msg = NotificationMessage "initialized" Null
                decode (encode msg) `shouldBe` Just msg
        context "when using JSON-RPC protocol helpers" $ do
            it "should encode message with Content-Length header" $ do
                let msg = NotificationMessage "test" Null
                    encoded = encodeLspMessage msg
                encoded `shouldSatisfy` (\bs -> "Content-Length:" `isInfixOf` show bs)
            it "should parse Content-Length from header" $ do
                let input = L8.pack "Content-Length: 42\r\n\r\n{}"
                parseContentLength input `shouldBe` Just 42
