-- | Explicit Graph JSON contract. Keys and tags are stable.
module MyOrg.Http.Codec.Graph
  ( nodeCodec
  , edgeKindCodec
  , edgeCodec
  , responsibilityGraphCodec
  ) where

import Data.Aeson (Value (String), withObject, withText)
import Data.Text qualified as T
import MyOrg.Domain.Graph
import MyOrg.Serialization.Codec
import MyOrg.Serialization.Identity

edgeCodec :: Codec Edge
edgeCodec = Codec encode decode
  where
    encode Edge {..} =
      record
        [ ("from", Just (encodeValue nodeCodec edgeFrom))
        , ("kind", Just (encodeValue edgeKindCodec edgeKind))
        , ("to", Just (encodeValue nodeCodec edgeTo))
        ]
    decode = withObject "Edge" $ \obj ->
      Edge
        <$> field nodeCodec obj "from"
        <*> field edgeKindCodec obj "kind"
        <*> field nodeCodec obj "to"

responsibilityGraphCodec :: Codec ResponsibilityGraph
responsibilityGraphCodec = Codec encode decode
  where
    encode ResponsibilityGraph {..} =
      record
        [ ("nodes", Just (encodeValue (setCodec nodeCodec) graphNodes))
        , ("edges", Just (encodeValue (listCodec edgeCodec) graphEdges))
        ]
    decode = withObject "ResponsibilityGraph" $ \obj ->
      ResponsibilityGraph
        <$> field (setCodec nodeCodec) obj "nodes"
        <*> field (listCodec edgeCodec) obj "edges"

edgeKindCodec :: Codec EdgeKind
edgeKindCodec = Codec encode decode
  where
    encode = \case
      Owns -> String "Owns"
      DependsOn -> String "DependsOn"
      Controls -> String "Controls"
      Measures -> String "Measures"
    decode = withText "EdgeKind" $ \tag -> case tag of
      "Owns"      -> pure Owns
      "DependsOn" -> pure DependsOn
      "Controls"  -> pure Controls
      "Measures"  -> pure Measures
      _           -> fail ("Unknown EdgeKind: " <> T.unpack tag)

nodeCodec :: Codec Node
nodeCodec = Codec encode decode
  where
    encode = \case
      PersonNode a -> tagged "PersonNode" (Just (encodeValue userIdCodec a))
      GoalNode a -> tagged "GoalNode" (Just (encodeValue goalIdCodec a))
      MetricNode a -> tagged "MetricNode" (Just (encodeValue metricIdCodec a))
      ResourceNode a -> tagged "ResourceNode" (Just (encodeValue resourceIdCodec a))
    decode = withObject "Node" $ \obj -> do
      tag <- field textCodec obj "tag"
      case tag of
        "PersonNode"   -> PersonNode <$> field userIdCodec obj "contents"
        "GoalNode"     -> GoalNode <$> field goalIdCodec obj "contents"
        "MetricNode"   -> MetricNode <$> field metricIdCodec obj "contents"
        "ResourceNode" -> ResourceNode <$> field resourceIdCodec obj "contents"
        _              -> fail ("Unknown Node: " <> T.unpack tag)
