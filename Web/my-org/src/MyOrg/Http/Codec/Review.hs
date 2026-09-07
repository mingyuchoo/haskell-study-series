-- | Explicit Review JSON contract. Keys and tags are stable.
module MyOrg.Http.Codec.Review
  ( reviewWarningCodec
  ) where

import Data.Aeson (withObject)
import Data.Text qualified as T
import MyOrg.Domain.Review
import MyOrg.Serialization.Codec

reviewWarningCodec :: Codec ReviewWarning
reviewWarningCodec = Codec encode decode
  where
    encode = \case
      NoDecisionProduced  -> tagged "NoDecisionProduced" Nothing
      DecisionWithoutDeadline a -> tagged "DecisionWithoutDeadline" (Just (encodeValue textCodec a))
    decode = withObject "ReviewWarning" $ \obj -> do
      tag <- field textCodec obj "tag"
      case tag of
        "NoDecisionProduced" -> pure NoDecisionProduced
        "DecisionWithoutDeadline" -> DecisionWithoutDeadline <$> field textCodec obj "contents"
        _ -> fail ("Unknown ReviewWarning: " <> T.unpack tag)
