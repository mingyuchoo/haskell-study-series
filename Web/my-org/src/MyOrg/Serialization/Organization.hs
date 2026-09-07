-- | Explicit Organization JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Organization
  ( organizationCodec
  , personCodec
  , employeeProfileCodec
  ) where

import Data.Aeson (withObject)
import MyOrg.Domain.Organization
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity

organizationCodec :: Codec Organization
organizationCodec = Codec encode decode
  where
    encode Organization {..} =
      record
        [ ("id", Just (encodeValue orgIdCodec organizationId))
        , ("name", Just (encodeValue textCodec organizationName))
        , ("createdAt", Just (encodeValue timeCodec organizationCreatedAt))
        ]
    decode = withObject "Organization" $ \obj ->
      Organization
        <$> field orgIdCodec obj "id"
        <*> field textCodec obj "name"
        <*> field timeCodec obj "createdAt"

personCodec :: Codec Person
personCodec = Codec encode decode
  where
    encode Person {..} =
      record
        [ ("id", Just (encodeValue userIdCodec personId))
        , ("name", Just (encodeValue textCodec personName))
        , ("role", Just (encodeValue textCodec personRole))
        , ("reportsTo", encodeValue userIdCodec <$> personReportsTo)
        ]
    decode = withObject "Person" $ \obj ->
      Person
        <$> field userIdCodec obj "id"
        <*> field textCodec obj "name"
        <*> field textCodec obj "role"
        <*> optionalField userIdCodec obj "reportsTo"

employeeProfileCodec :: Codec EmployeeProfile
employeeProfileCodec = Codec encode decode
  where
    encode EmployeeProfile {..} =
      record
        [ ("department", encodeValue textCodec <$> profileDepartment)
        , ("email", encodeValue textCodec <$> profileEmail)
        ]
    decode = withObject "EmployeeProfile" $ \obj ->
      EmployeeProfile
        <$> optionalField textCodec obj "department"
        <*> optionalField textCodec obj "email"
