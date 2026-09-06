module WireSpec
  ( spec
  ) where

import Data.Aeson (Value)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import MyOrg.Domain.Authority
import MyOrg.Domain.Compiler
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Result
import MyOrg.Serialization.JSON
import Test.Hspec

spec :: Spec
spec = describe "explicit boundary codecs" $ do
  it "preserves nullary and three-argument sum contracts" $ do
    contract NoOrganization "{\"tag\":\"NoOrganization\"}"
    contract
      (MissingPermissions (GoalId "g") (UserId "u") (Set.fromList [Pricing, Hiring]))
      "{\"tag\":\"MissingPermissions\",\"contents\":[\"g\",\"u\",[\"Pricing\",\"Hiring\"]]}"
  it "rejects unknown enum/constructor tags and wrong tuple arity" $ do
    (eitherDecodeWire "\"NewStatus\"" :: Either String GoalStatus) `shouldSatisfy` isLeft
    (eitherDecodeWire "\"RootAccess\"" :: Either String Permission) `shouldSatisfy` isLeft
    (eitherDecodeWire "{\"tag\":\"UnknownEvent\"}" :: Either String OrganizationEvent)
      `shouldSatisfy` isLeft
    ( eitherDecodeWire "{\"tag\":\"OwnerAssigned\",\"contents\":[\"g\",\"u\",\"extra\"]}"
        :: Either String OrganizationEvent
      )
      `shouldSatisfy` isLeft
    ( eitherDecodeWire "{\"tag\":\"OwnerAssigned\",\"contents\":[\"g\"]}"
        :: Either String OrganizationEvent
      )
      `shouldSatisfy` isLeft
  it "accepts absent/null optional values, omits them on write, and rejects mistypes" $ do
    let person = Person (UserId "u") "Name" "Role" Nothing
        absent = "{\"id\":\"u\",\"name\":\"Name\",\"role\":\"Role\"}"
    contract person absent
    eitherDecodeWire "{\"id\":\"u\",\"name\":\"Name\",\"role\":\"Role\",\"reportsTo\":null}"
      `shouldBe` Right person
    ( eitherDecodeWire "{\"id\":\"u\",\"name\":\"Name\",\"role\":\"Role\",\"reportsTo\":42}"
        :: Either String Person
      )
      `shouldSatisfy` isLeft
    (eitherDecodeWire "{\"id\":\"u\",\"name\":\"Name\"}" :: Either String Person)
      `shouldSatisfy` isLeft
  it "keeps user maps as JSON objects with literal Unicode keys" $
    contract
      (Map.fromList [(UserId "owner", 0.75 :: Double), (UserId "한글", 0.25)])
      "{\"owner\":0.75,\"한글\":0.25}"
  it "renders structured draft errors as the existing diagnostic text projection" $ do
    let diagnostic = Diagnostic "O009" Error "goal" (InvalidDraft (InvalidInput "텍스트는 1~10000자여야 합니다.")) []
        raw =
          BL.fromStrict
            ( TE.encodeUtf8
                "{\"code\":\"O009\",\"severity\":\"Error\",\"subject\":\"goal\",\"message\":\"텍스트는 1~10000자여야 합니다.\",\"details\":[]}"
            )
    value <- either fail pure (A.eitherDecode raw)
    toWire diagnostic `shouldBe` (value :: Value)
    decoded <- either fail pure (eitherDecodeWire raw)
    -- HTTP text is a projection: historical JSON cannot recreate its typed cause.
    diagnosticMessage decoded `shouldBe` PlainMessage "텍스트는 1~10000자여야 합니다."
    toWire decoded `shouldBe` value
  it "keeps primitive identifiers and money unwrapped" $ do
    contract (OrgId "org") "\"org\""
    contract (Money 1000000000000000000) "1000000000000000000"
    contract (Nothing :: Maybe Text) "null"

contract :: (Eq a, Show a, Wire a) => a -> Text -> Expectation
contract value text = do
  let raw = BL.fromStrict (TE.encodeUtf8 text)
  eitherDecodeWire raw `shouldBe` Right value
  json <- either fail pure (A.eitherDecode raw)
  toWire value `shouldBe` (json :: Value)
