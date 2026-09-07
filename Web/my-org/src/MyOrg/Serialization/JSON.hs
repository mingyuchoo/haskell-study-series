-- | Compatibility facade for existing callers. New boundary adapters select
-- MyOrg.Http.Codec or MyOrg.Serialization.Persistence explicitly.
module MyOrg.Serialization.JSON
  ( module MyOrg.Http.Codec
  ) where

import MyOrg.Http.Codec
