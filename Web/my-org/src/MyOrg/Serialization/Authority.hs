-- | Explicit Authority JSON contract. Keys and tags are stable.
module MyOrg.Serialization.Authority
  ( permissionCodec
  , ownershipCodec
  , authorityCodec
  ) where

import Data.Aeson (Value (String), withObject, withText)
import Data.Text qualified as T
import MyOrg.Domain.Authority
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity

ownershipCodec :: Codec Ownership
ownershipCodec = Codec encode decode
  where
    encode Ownership {..} =
      record
        [ ("goal", Just (encodeValue goalIdCodec ownershipGoal))
        , ("owner", Just (encodeValue userIdCodec ownershipOwner))
        , ("since", Just (encodeValue timeCodec ownershipSince))
        ]
    decode = withObject "Ownership" $ \obj ->
      Ownership
        <$> field goalIdCodec obj "goal"
        <*> field userIdCodec obj "owner"
        <*> field timeCodec obj "since"

authorityCodec :: Codec Authority
authorityCodec = Codec encode decode
  where
    encode Authority {..} =
      record
        [ ("owner", Just (encodeValue userIdCodec authorityOwner))
        , ("budgetLimit", Just (encodeValue moneyCodec authorityBudgetLimit))
        , ("canHire", Just (encodeValue boolCodec authorityCanHire))
        , ("canChangePrice", Just (encodeValue boolCodec authorityCanChangePrice))
        , ("canApprove", Just (encodeValue (setCodec permissionCodec) authorityCanApprove))
        ]
    decode = withObject "Authority" $ \obj ->
      Authority
        <$> field userIdCodec obj "owner"
        <*> field moneyCodec obj "budgetLimit"
        <*> field boolCodec obj "canHire"
        <*> field boolCodec obj "canChangePrice"
        <*> field (setCodec permissionCodec) obj "canApprove"

permissionCodec :: Codec Permission
permissionCodec = Codec encode decode
  where
    encode = \case
      Pricing -> String "Pricing"
      Hiring -> String "Hiring"
      BudgetApproval -> String "BudgetApproval"
      Contracting -> String "Contracting"
      Marketing -> String "Marketing"
      Infrastructure -> String "Infrastructure"
      ProductLaunch -> String "ProductLaunch"
    decode = withText "Permission" $ \tag -> case tag of
      "Pricing"        -> pure Pricing
      "Hiring"         -> pure Hiring
      "BudgetApproval" -> pure BudgetApproval
      "Contracting"    -> pure Contracting
      "Marketing"      -> pure Marketing
      "Infrastructure" -> pure Infrastructure
      "ProductLaunch"  -> pure ProductLaunch
      _                -> fail ("Unknown Permission: " <> T.unpack tag)
