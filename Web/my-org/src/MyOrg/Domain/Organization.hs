-- | Pure Organization domain concepts.
module MyOrg.Domain.Organization
  ( Organization (..)
  , Person (..)
  , EmployeeProfile (..)
  , emptyProfile
  , handoverReports
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

-- | Optional employee data is separate from the historical Person contract.
data EmployeeProfile = EmployeeProfile
  { profileDepartment :: Maybe Text
  , profileEmail      :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

emptyProfile :: EmployeeProfile
emptyProfile = EmployeeProfile Nothing Nothing

-- | Promote a direct-report successor before moving the remaining reports.
handoverReports :: Person -> UserId -> Person -> Person
handoverReports departing successor p
  | personId p == personId departing = p {personReportsTo = Nothing}
  | personId p == successor && personReportsTo p == Just (personId departing) =
      p {personReportsTo = personReportsTo departing}
  | personReportsTo p == Just (personId departing) = p {personReportsTo = Just successor}
  | otherwise = p
