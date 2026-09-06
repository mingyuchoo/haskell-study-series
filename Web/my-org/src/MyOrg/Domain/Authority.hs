-- | Pure Authority domain concepts.
module MyOrg.Domain.Authority (Ownership(..), Permission(..), Authority(..), allPermissions, emptyAuthority, grantedPermissions, hasPermission, revokePermission) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Identity

-- | 목표 하나에는 최종 책임자가 정확히 한 명이다.
data Ownership = Ownership
  { ownershipGoal :: GoalId
  , ownershipOwner :: UserId
  , ownershipSince :: UTCTime
  }
  deriving stock (Show, Eq, Generic)



-- | 조직 안에서 독자적으로 내릴 수 있는 결정의 종류.
data Permission
  = Pricing
  | Hiring
  | BudgetApproval
  | Contracting
  | Marketing
  | Infrastructure
  | ProductLaunch
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)

allPermissions :: Set Permission
allPermissions = Set.fromList [minBound .. maxBound]

-- | 한 사람이 가진 권한의 집합.
--
-- 'authorityCanHire'와 'authorityCanChangePrice'는 각각 'Hiring', 'Pricing'
-- 권한의 별칭이다. 'grantedPermissions'가 둘을 합쳐 하나의 집합으로 만든다.
data Authority = Authority
  { authorityOwner :: UserId
  , authorityBudgetLimit :: Money
  , authorityCanHire :: Bool
  , authorityCanChangePrice :: Bool
  , authorityCanApprove :: Set Permission
  }
  deriving stock (Show, Eq, Generic)



-- | 아무 권한도 없는 상태.
emptyAuthority :: UserId -> Authority
emptyAuthority uid =
  Authority
    { authorityOwner = uid
    , authorityBudgetLimit = Money 0
    , authorityCanHire = False
    , authorityCanChangePrice = False
    , authorityCanApprove = Set.empty
    }

-- | 불리언 플래그와 승인 집합을 합친 실제 권한 집합.
grantedPermissions :: Authority -> Set Permission
grantedPermissions Authority{..} =
  Set.unions
    [ authorityCanApprove
    , if authorityCanHire then Set.singleton Hiring else Set.empty
    , if authorityCanChangePrice then Set.singleton Pricing else Set.empty
    ]

hasPermission :: Authority -> Permission -> Bool
hasPermission a p = Set.member p (grantedPermissions a)

-- | 권한 하나를 회수한다. 플래그와 집합 양쪽에서 제거한다.
revokePermission :: Permission -> Authority -> Authority
revokePermission p a =
  a
    { authorityCanApprove = Set.delete p (authorityCanApprove a)
    , authorityCanHire = authorityCanHire a && p /= Hiring
    , authorityCanChangePrice = authorityCanChangePrice a && p /= Pricing
    }
