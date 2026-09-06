-- | 조직 운영체계의 핵심 타입.
--
-- 이 모듈은 @Task@가 아니라 다섯 개의 중심 객체를 정의한다.
--
-- > Organization -> Goal -> Ownership -> Authority -> Result
--
-- 활성 목표(ActiveGoal)는 스마트 생성자 'MyOrg.Domain.Goal.activate'를 통해서만
-- 만들 수 있으므로, "책임자 없는 활성 목표"나 "권한 없는 책임자" 같은 상태는
-- 타입 차원에서 표현할 수 없다.
module MyOrg.Types
  ( -- * 식별자
    OrgId (..)
  , UserId (..)
  , GoalId (..)
  , MetricId (..)
  , ResourceId (..)
  , ReviewId (..)
  , Money (..)

    -- * 조직과 사람
  , Organization (..)
  , Person (..)

    -- * 목표와 지표
  , MetricDirection (..)
  , Metric (..)
  , Goal (..)

    -- * 책임과 권한
  , Ownership (..)
  , Permission (..)
  , allPermissions
  , Authority (..)
  , emptyAuthority
  , grantedPermissions
  , hasPermission
  , revokePermission

    -- * 결과, 평가, 학습
  , Result (..)
  , GoalStatus (..)
  , Evaluation (..)
  , Learning (..)
  , Decision (..)
  , Review (..)

    -- * 오류
  , OrganizationError (..)
  , describeError

    -- * JSON 도우미
  , jsonOptions
  ) where

import Data.Aeson
  ( FromJSON (..)
  , FromJSONKey
  , Options (..)
  , ToJSON (..)
  , ToJSONKey
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- JSON 도우미

-- | 레코드 필드 접두사를 제거하고 첫 글자를 소문자로 만드는 aeson 옵션.
--
-- >>> fieldLabelModifier (jsonOptions "goal") "goalTarget"
-- "target"
jsonOptions :: String -> Options
jsonOptions prefix =
  defaultOptions
    { fieldLabelModifier = lowerFirst . dropPrefix
    , omitNothingFields = True
    }
 where
  dropPrefix s = maybe s id (stripPrefix prefix s)
  lowerFirst [] = []
  lowerFirst (c : cs) = toLower c : cs

-- ---------------------------------------------------------------------------
-- 식별자

newtype OrgId = OrgId {unOrgId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype UserId = UserId {unUserId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype GoalId = GoalId {unGoalId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype MetricId = MetricId {unMetricId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ResourceId = ResourceId {unResourceId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ReviewId = ReviewId {unReviewId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | 금액. 통화 단위는 조직 단위로 고정한다(기본 KRW).
newtype Money = Money {unMoney :: Integer}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Num, ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- 조직과 사람

data Organization = Organization
  { organizationId :: OrgId
  , organizationName :: Text
  , organizationCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Organization where
  toJSON = genericToJSON (jsonOptions "organization")

instance FromJSON Organization where
  parseJSON = genericParseJSON (jsonOptions "organization")

data Person = Person
  { personId :: UserId
  , personName :: Text
  , personRole :: Text
  , personReportsTo :: Maybe UserId
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Person where
  toJSON = genericToJSON (jsonOptions "person")

instance FromJSON Person where
  parseJSON = genericParseJSON (jsonOptions "person")

-- ---------------------------------------------------------------------------
-- 목표와 지표

-- | 지표의 방향. 매출은 높을수록 좋고, 이탈률은 낮을수록 좋다.
data MetricDirection = HigherIsBetter | LowerIsBetter
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metric = Metric
  { metricId :: MetricId
  , metricName :: Text
  , metricUnit :: Text
  , metricDirection :: MetricDirection
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Metric where
  toJSON = genericToJSON (jsonOptions "metric")

instance FromJSON Metric where
  parseJSON = genericParseJSON (jsonOptions "metric")

-- | 목표 정의. 이 자체로는 초안(Draft)이며, 책임자와 권한이 붙어야 활성화된다.
data Goal = Goal
  { goalId :: GoalId
  , goalOrganization :: OrgId
  , goalDescription :: Text
  , goalMetric :: Metric
  , goalBaseline :: Double
  -- ^ 시작 시점의 지표 값
  , goalTarget :: Double
  -- ^ 마감 시점에 도달해야 하는 값
  , goalStartsAt :: UTCTime
  , goalDeadline :: UTCTime
  , goalParent :: Maybe GoalId
  -- ^ 상위 목표. 상위 목표는 하위 목표에 의존한다(DependsOn).
  , goalRequiredPermissions :: Set Permission
  -- ^ 이 목표를 달성하기 위해 책임자가 가져야 하는 결정 권한
  , goalRequiredBudget :: Money
  -- ^ 이 목표를 달성하기 위해 책임자가 집행할 수 있어야 하는 예산
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Goal where
  toJSON = genericToJSON (jsonOptions "goal")

instance FromJSON Goal where
  parseJSON = genericParseJSON (jsonOptions "goal")

-- ---------------------------------------------------------------------------
-- 책임과 권한

-- | 목표 하나에는 최종 책임자가 정확히 한 명이다.
data Ownership = Ownership
  { ownershipGoal :: GoalId
  , ownershipOwner :: UserId
  , ownershipSince :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Ownership where
  toJSON = genericToJSON (jsonOptions "ownership")

instance FromJSON Ownership where
  parseJSON = genericParseJSON (jsonOptions "ownership")

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
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

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

instance ToJSON Authority where
  toJSON = genericToJSON (jsonOptions "authority")

instance FromJSON Authority where
  parseJSON = genericParseJSON (jsonOptions "authority")

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

-- ---------------------------------------------------------------------------
-- 결과, 평가, 학습

-- | 지표의 실측값 보고.
data Result = Result
  { resultGoal :: GoalId
  , resultValue :: Double
  , resultReportedAt :: UTCTime
  , resultReportedBy :: UserId
  , resultNote :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Result where
  toJSON = genericToJSON (jsonOptions "result")

instance FromJSON Result where
  parseJSON = genericParseJSON (jsonOptions "result")

data GoalStatus
  = NoData
  -- ^ 보고된 결과가 없음
  | OnTrack
  -- ^ 정상
  | AtRisk
  -- ^ 위험
  | OffTrack
  -- ^ 이탈
  | Achieved
  -- ^ 달성
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Evaluation = Evaluation
  { evaluationGoal :: GoalId
  , evaluationStatus :: GoalStatus
  , evaluationProgress :: Double
  -- ^ 0 = baseline, 1 = target. 실제 달성 비율
  , evaluationExpectedProgress :: Double
  -- ^ 시간 경과 기준으로 기대되는 비율
  , evaluationLatestValue :: Maybe Double
  , evaluationEvaluatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Evaluation where
  toJSON = genericToJSON (jsonOptions "evaluation")

instance FromJSON Evaluation where
  parseJSON = genericParseJSON (jsonOptions "evaluation")

newtype Learning = Learning {learningText :: Text}
  deriving stock (Show, Eq, Generic)

instance ToJSON Learning where
  toJSON = genericToJSON (jsonOptions "learning")

instance FromJSON Learning where
  parseJSON = genericParseJSON (jsonOptions "learning")

-- | 회의의 출력 중 하나. 결정에는 반드시 담당자가 있다.
data Decision = Decision
  { decisionText :: Text
  , decisionOwner :: UserId
  , decisionDeadline :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Decision where
  toJSON = genericToJSON (jsonOptions "decision")

instance FromJSON Decision where
  parseJSON = genericParseJSON (jsonOptions "decision")

-- | 리뷰 회의는 캘린더 이벤트가 아니라 데이터 구조다.
data Review = Review
  { reviewId :: ReviewId
  , reviewGoal :: GoalId
  , reviewResult :: Maybe Result
  , reviewEvaluation :: Evaluation
  , reviewLearnings :: [Learning]
  , reviewDecisions :: [Decision]
  , reviewHeldAt :: UTCTime
  , reviewNote :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Review where
  toJSON = genericToJSON (jsonOptions "review")

instance FromJSON Review where
  parseJSON = genericParseJSON (jsonOptions "review")

-- ---------------------------------------------------------------------------
-- 오류

-- | 조직 상태가 규칙을 어길 때 발생하는 오류.
data OrganizationError
  = NoOrganization
  | AmbiguousOrganizations
  | OrganizationNotFound OrgId
  | VersionConflict Int Int
  | OrganizationAlreadyExists
  | GoalNotFound GoalId
  | PersonNotFound UserId
  | NoOwner GoalId
  | OwnerMismatch GoalId UserId UserId
  -- ^ 목표, 책임자, 권한 소유자가 서로 다름
  | NoAuthority UserId
  | MissingPermissions GoalId UserId (Set Permission)
  | InsufficientBudget GoalId Money Money
  -- ^ 목표, 필요 예산, 부여된 예산
  | InvalidTarget GoalId
  | DeadlineBeforeStart GoalId
  | GoalAlreadyActive GoalId
  | GoalNotActive GoalId
  | ParentGoalNotFound GoalId GoalId
  | StorageFailure
  | InvalidInput Text
  | DuplicateId Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 사용자에게 보여줄 오류 설명.
describeError :: OrganizationError -> Text
describeError = \case
  NoOrganization -> "조직이 아직 생성되지 않았습니다."
  AmbiguousOrganizations -> "조직이 여러 개입니다. /api/organizations/:id 경로로 대상 조직을 지정하세요."
  OrganizationNotFound oid -> "조직을 찾을 수 없습니다: " <> unOrgId oid
  VersionConflict _ _ -> "확인 중 조직이 변경되었습니다. 최신 상태를 확인하고 다시 시도해주세요."
  OrganizationAlreadyExists -> "조직이 이미 존재합니다."
  GoalNotFound g -> "목표를 찾을 수 없습니다: " <> unGoalId g
  PersonNotFound u -> "구성원을 찾을 수 없습니다: " <> unUserId u
  NoOwner g -> "최종 책임자(Final Owner)가 존재하지 않습니다: " <> unGoalId g
  OwnerMismatch g o a ->
    "목표 " <> unGoalId g <> "의 책임자(" <> unUserId o
      <> ")와 권한 소유자(" <> unUserId a <> ")가 다릅니다."
  NoAuthority u -> "권한 기록이 없습니다: " <> unUserId u
  MissingPermissions g u ps ->
    "목표 " <> unGoalId g <> "의 책임자 " <> unUserId u <> "에게 다음 권한이 없습니다: "
      <> T.intercalate ", " (map (T.pack . show) (Set.toList ps))
  InsufficientBudget g (Money need) (Money have) ->
    "목표 " <> unGoalId g <> "에 필요한 예산 " <> T.pack (show need)
      <> " 대비 부여된 예산이 " <> T.pack (show have) <> "입니다."
  InvalidTarget g -> "목표값이 기준값과 같아 성공을 판단할 수 없습니다: " <> unGoalId g
  DeadlineBeforeStart g -> "마감이 시작일보다 앞섭니다: " <> unGoalId g
  GoalAlreadyActive g -> "이미 활성화된 목표입니다: " <> unGoalId g
  GoalNotActive g -> "활성화되지 않은 목표입니다: " <> unGoalId g
  ParentGoalNotFound g p ->
    "목표 " <> unGoalId g <> "의 상위 목표를 찾을 수 없습니다: " <> unGoalId p
  StorageFailure -> "저장에 실패했습니다. 상태를 변경하지 않았습니다."
  InvalidInput t -> t
  DuplicateId t -> "이미 사용 중인 식별자입니다: " <> t
