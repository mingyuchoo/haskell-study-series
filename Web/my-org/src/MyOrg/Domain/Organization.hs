-- | Pure Organization domain concepts.
module MyOrg.Domain.Organization
  ( Organization (..)
  , Person (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Identity

data Organization = Organization
  { organizationId        :: OrgId
  , organizationName      :: Text
  , organizationCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

data Person = Person
  { personId        :: UserId
  , personName      :: Text
  , personRole      :: Text
  , personReportsTo :: Maybe UserId
  }
  deriving stock (Show, Eq, Generic)
