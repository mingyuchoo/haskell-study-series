-- | 조직의 모든 상태 변화는 이벤트로 남긴다.
--
-- 현재 상태('OrgState')는 이벤트 목록을 왼쪽부터 접은 결과다.
-- 따라서 "누가 언제 무엇을 결정했는가"를 언제든 다시 재생할 수 있다.
module MyOrg.Domain.Event
  ( OrganizationEvent (..)
  , StoredEvent (..)
  , OrgState (..)
  , emptyState
  , applyEvent
  , applyEvents
  , replay
  , activeGoals
  , draftGoals
  , goalOwnership
  , goalOwner
  , ownerAuthority
  , resultsOf
  , validateActive
  , currentEpoch
  , describeEvent
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import MyOrg.Domain.Goal (ActiveGoal, validateGoal)
import MyOrg.Types

data OrganizationEvent
  = OrganizationCreated Organization
  | OrganizationScoped OrgId OrganizationEvent
  | OrganizationRenamed OrgId Text
  | OrganizationDeleted OrgId
  | DemoSeeded OrgId
  | PersonAdded Person
  | GoalCreated Goal
  | OwnerAssigned GoalId UserId
  | AuthorityGranted UserId Authority
  | AuthorityRevoked UserId Permission
  | GoalActivated GoalId
  | ResultReported GoalId Result
  | GoalEvaluated GoalId Evaluation
  | ReviewHeld Review
  | StrategyChanged GoalId Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 저장소에 기록된 이벤트. 순번과 시각, 행위자를 함께 남긴다.
data StoredEvent = StoredEvent
  { storedSeq :: Int
  , storedAt :: UTCTime
  , storedActor :: Maybe UserId
  , storedEvent :: OrganizationEvent
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON StoredEvent where
  toJSON = genericToJSON (jsonOptions "stored")

instance FromJSON StoredEvent where
  parseJSON = genericParseJSON (jsonOptions "stored")

-- | 이벤트를 접어 만든 현재 상태.
data OrgState = OrgState
  { stateOrganization :: Maybe Organization
  , statePeople :: Map UserId Person
  , stateGoals :: Map GoalId Goal
  , stateOwnership :: Map GoalId Ownership
  , stateAuthorities :: Map UserId Authority
  , stateActive :: Set GoalId
  , stateResults :: Map GoalId [Result]
  -- ^ 최신 결과가 앞에 온다
  , stateEvaluations :: Map GoalId Evaluation
  , stateReviews :: [Review]
  -- ^ 최신 리뷰가 앞에 온다
  , stateStrategies :: Map GoalId [(UTCTime, Text)]
  , stateLastSeq :: Int
  }
  deriving stock (Show, Eq, Generic)

emptyState :: OrgState
emptyState =
  OrgState
    { stateOrganization = Nothing
    , statePeople = Map.empty
    , stateGoals = Map.empty
    , stateOwnership = Map.empty
    , stateAuthorities = Map.empty
    , stateActive = Set.empty
    , stateResults = Map.empty
    , stateEvaluations = Map.empty
    , stateReviews = []
    , stateStrategies = Map.empty
    , stateLastSeq = 0
    }

applyEvent :: OrgState -> StoredEvent -> OrgState
applyEvent st StoredEvent{storedSeq, storedAt, storedEvent} =
  let next = (step storedEvent) {stateLastSeq = max (stateLastSeq st) storedSeq}
   in next {stateActive = Set.filter (\gid -> either (const False) (const True) (validateActive next gid)) (stateActive next)}
 where
  step = \case
    OrganizationCreated o -> emptyState {stateOrganization = Just o}
    OrganizationScoped _ event -> step event
    OrganizationRenamed _ name -> st {stateOrganization = fmap (\org -> org {organizationName = name}) (stateOrganization st)}
    OrganizationDeleted _ -> emptyState
    DemoSeeded _ -> st
    PersonAdded p -> st {statePeople = Map.insert (personId p) p (statePeople st)}
    GoalCreated g -> st {stateGoals = Map.insert (goalId g) g (stateGoals st)}
    OwnerAssigned gid uid ->
      st
        { stateOwnership =
            Map.insert gid (Ownership gid uid storedAt) (stateOwnership st)
        , -- 책임자가 바뀌면 활성 상태는 다시 검증해야 하므로 해제한다.
          stateActive = Set.delete gid (stateActive st)
        }
    AuthorityGranted uid a ->
      st {stateAuthorities = Map.insert uid a {authorityOwner = uid} (stateAuthorities st)}
    AuthorityRevoked uid p ->
      st {stateAuthorities = Map.adjust (revokePermission p) uid (stateAuthorities st)}
    GoalActivated gid -> st {stateActive = Set.insert gid (stateActive st)}
    ResultReported gid r ->
      st {stateResults = Map.insertWith (++) gid [r] (stateResults st)}
    GoalEvaluated gid e -> st {stateEvaluations = Map.insert gid e (stateEvaluations st)}
    ReviewHeld r -> st {stateReviews = r : stateReviews st}
    StrategyChanged gid t ->
      st {stateStrategies = Map.insertWith (++) gid [(storedAt, t)] (stateStrategies st)}

applyEvents :: OrgState -> [StoredEvent] -> OrgState
applyEvents = foldl' applyEvent

replay :: [StoredEvent] -> OrgState
replay = applyEvents emptyState

-- | 활성화된 목표 목록.
activeGoals :: OrgState -> [Goal]
activeGoals st =
  [g | g <- Map.elems (stateGoals st), Set.member (goalId g) (stateActive st)]

draftGoals :: OrgState -> [Goal]
draftGoals st =
  [g | g <- Map.elems (stateGoals st), not (Set.member (goalId g) (stateActive st))]

goalOwnership :: OrgState -> GoalId -> Maybe Ownership
goalOwnership st gid = Map.lookup gid (stateOwnership st)

goalOwner :: OrgState -> GoalId -> Maybe UserId
goalOwner st gid = ownershipOwner <$> goalOwnership st gid

ownerAuthority :: OrgState -> UserId -> Maybe Authority
ownerAuthority st uid = Map.lookup uid (stateAuthorities st)

resultsOf :: OrgState -> GoalId -> [Result]
resultsOf st gid = Map.findWithDefault [] gid (stateResults st)

-- | 현재 상태에서 목표를 활성화할 수 있는지 검증한다.
validateActive :: OrgState -> GoalId -> Either OrganizationError ActiveGoal
validateActive st gid = do
  g <- maybe (Left (GoalNotFound gid)) Right (Map.lookup gid (stateGoals st))
  let o = goalOwnership st gid
      a = o >>= ownerAuthority st . ownershipOwner
  case o of
    Just own | not (Map.member (ownershipOwner own) (statePeople st)) -> Left (PersonNotFound (ownershipOwner own))
    _ -> validateGoal g o a

-- | 타임라인에 표시할 한 줄 설명.
describeEvent :: OrganizationEvent -> Text
describeEvent = \case
  OrganizationScoped _ event -> describeEvent event
  OrganizationRenamed oid name -> "조직 이름 변경: " <> unOrgId oid <> " → " <> name
  OrganizationDeleted oid -> "조직 논리 삭제: " <> unOrgId oid
  DemoSeeded _ -> "체험용 데모 시드 생성 완료"
  OrganizationCreated o -> "조직 생성: " <> organizationName o
  PersonAdded p -> "구성원 추가: " <> personName p <> " (" <> personRole p <> ")"
  GoalCreated g -> "목표 생성: " <> goalDescription g
  OwnerAssigned g u -> "책임자 지정: " <> unGoalId g <> " -> " <> unUserId u
  AuthorityGranted u a ->
    "권한 부여: " <> unUserId u <> " = "
      <> T.intercalate ", " (map (T.pack . show) (Set.toList (grantedPermissions a)))
      <> ", 예산 " <> T.pack (show (unMoney (authorityBudgetLimit a)))
  AuthorityRevoked u p -> "권한 회수: " <> unUserId u <> " - " <> T.pack (show p)
  GoalActivated g -> "목표 활성화: " <> unGoalId g
  ResultReported g r ->
    "결과 보고: " <> unGoalId g <> " = " <> T.pack (show (resultValue r))
  GoalEvaluated g e ->
    "평가: " <> unGoalId g <> " = " <> T.pack (show (evaluationStatus e))
  ReviewHeld r ->
    "리뷰: " <> unGoalId (reviewGoal r) <> ", 결정 "
      <> T.pack (show (length (reviewDecisions r))) <> "건, 학습 "
      <> T.pack (show (length (reviewLearnings r))) <> "건"
  StrategyChanged g t -> "전략 변경: " <> unGoalId g <> " - " <> t

-- | Only the active organization's lifecycle is visible to ordinary API reads.
-- The underlying append-only audit remains intact across deletion/recreation.
currentEpoch :: [StoredEvent] -> [StoredEvent]
currentEpoch = reverse . foldl step []
 where
  step _ event@StoredEvent{storedEvent = OrganizationCreated _} = [event]
  step _ StoredEvent{storedEvent = OrganizationDeleted _} = []
  step [] _ = []
  step events event = event : events
