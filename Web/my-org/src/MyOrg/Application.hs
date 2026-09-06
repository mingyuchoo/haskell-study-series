module MyOrg.Application
  ( Command (..)
  , executeCommand
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Evaluation (evaluateGoal, latestResult)
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal (validateDraft)
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Queries
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types
import MyOrg.Domain.State
import MyOrg.Domain.Validation

data Command = CreateOrganization OrgId Text
             | RenameOrganization OrgId Text Int
             | DeleteOrganization OrgId Text Int
             | AddPerson Person
             | AddEmployee Person EmployeeProfile
             | UpdatePerson Person EmployeeProfile Int
             | DeactivatePerson UserId (Maybe UserId) Int
             | CreateGoal Goal
             | AssignOwner GoalId UserId
             | GrantGoalAuthority GoalId Authority
             | GrantAuthority Authority
             | RevokeAuthority UserId Permission
             | ActivateGoal GoalId
             | ReportResult GoalId Double UserId Text
             | EvaluateGoal GoalId
             | HoldReview ReviewId GoalId [Learning] [Decision] Text
             | ChangeStrategy GoalId Text
  deriving (Show, Eq)

executeCommand
  :: UTCTime -> OrgState -> Command -> Either OrganizationError [OrganizationEvent]
executeCommand now st command = fmap pure $ case command of
  CreateOrganization oid name -> do
    when (stateOrganization st /= Nothing) (Left OrganizationAlreadyExists)
    identifier (unOrgId oid)
    nonempty name
    pure (OrganizationCreated (Organization oid name now))
  RenameOrganization oid name expectedVersion -> do
    org <- maybe (Left (OrganizationNotFound oid)) Right (stateOrganization st)
    unless (organizationId org == oid) (Left (OrganizationNotFound oid))
    unless
      (expectedVersion == stateLastSeq st)
      (Left (VersionConflict expectedVersion (stateLastSeq st)))
    nonempty name
    pure (OrganizationRenamed oid name)
  DeleteOrganization oid confirmName expectedVersion -> do
    org <- maybe (Left (OrganizationNotFound oid)) Right (stateOrganization st)
    unless (organizationId org == oid) (Left (OrganizationNotFound oid))
    unless
      (expectedVersion == stateLastSeq st)
      (Left (VersionConflict expectedVersion (stateLastSeq st)))
    unless
      (confirmName == organizationName org)
      (Left (InvalidInput "확인 이름이 현재 조직명과 정확히 일치해야 합니다."))
    pure (OrganizationDeleted oid)
  AddPerson p -> do
    organization
    identifier (unUserId (personId p))
    nonempty (personName p)
    nonempty (personRole p)
    duplicate (Map.member (personId p) (statePeople st)) (unUserId (personId p))
    mapM_ person (personReportsTo p)
    pure (PersonAdded p)
  AddEmployee p profile -> do
    _ <- executeCommand now st (AddPerson p)
    cleaned <- validateProfile profile
    pure (if cleaned == emptyProfile then PersonAdded p else EmployeeAdded p cleaned)
  UpdatePerson p profile version -> do
    checkVersion st version
    unless (Map.member (personId p) (statePeople st)) (Left (PersonNotFound (personId p)))
    nonempty (personName p)
    nonempty (personRole p)
    mapM_ person (personReportsTo p)
    validateReports (Map.insert (personId p) p (statePeople st))
    cleaned <- validateProfile profile
    pure (PersonUpdated p cleaned)
  DeactivatePerson uid successor version -> do
    checkVersion st version
    person uid
    let departing = statePeople st Map.! uid
        owns = any ((== uid) . ownershipOwner) (Map.elems (stateOwnership st))
        reports = any ((== Just uid) . personReportsTo) (Map.elems (statePeople st))
    when
      ((owns || reports) && successor == Nothing)
      (Left (InvalidInput "담당 목표 또는 직속 보고자가 있어 활성 인계 대상이 필요합니다."))
    mapM_
      ( \next -> do
          when (next == uid) (Left (InvalidInput "자기 자신에게 인계할 수 없습니다."))
          person next
          validateReports (Map.map (handoverReports departing next) (statePeople st))
      )
      successor
    pure (PersonDeactivated uid successor)
  CreateGoal g -> do
    org <- maybe (Left NoOrganization) Right (stateOrganization st)
    unless
      (goalOrganization g == organizationId org)
      (Left (InvalidInput "목표의 조직이 일치하지 않습니다."))
    identifier (unGoalId (goalId g))
    duplicate (Map.member (goalId g) (stateGoals st)) (unGoalId (goalId g))
    validateDraft g
    mapM_
      (\p -> unless (Map.member p (stateGoals st)) (Left (ParentGoalNotFound (goalId g) p)))
      (goalParent g)
    pure (GoalCreated g)
  AssignOwner gid uid -> goal gid >> person uid >> pure (OwnerAssigned gid uid)
  GrantGoalAuthority gid a -> do
    _ <- goal gid
    owner <- maybe (Left (NoOwner gid)) Right (goalOwner st gid)
    unless (owner == authorityOwner a) (Left (OwnerMismatch gid owner (authorityOwner a)))
    case executeCommand now st (GrantAuthority a) of
      Right [event] -> pure event
      Left err      -> Left err
      _             -> Left (InvalidInput "잘못된 권한 명령입니다.")
  GrantAuthority a -> do
    person (authorityOwner a)
    when (authorityBudgetLimit a < 0) (Left (InvalidInput "예산은 음수일 수 없습니다."))
    pure (AuthorityGranted (authorityOwner a) a)
  RevokeAuthority uid permission -> person uid >> pure (AuthorityRevoked uid permission)
  ActivateGoal gid -> do
    when (Set.member gid (stateActive st)) (Left (GoalAlreadyActive gid))
    _ <- validateActive st gid
    pure (GoalActivated gid)
  ReportResult gid value uid note -> do
    _ <- goal gid
    active gid
    person uid
    when (isNaN value || isInfinite value) (Left (InvalidInput "결과는 유한한 숫자여야 합니다."))
    nonempty note
    pure (ResultReported gid (Result gid value now uid note))
  EvaluateGoal gid -> do
    g <- goal gid
    active gid
    pure (GoalEvaluated gid (evaluateGoal now g (resultsOf st gid)))
  HoldReview rid gid learnings decisions note -> do
    g <- goal gid
    active gid
    identifier (unReviewId rid)
    duplicate (any ((== rid) . reviewId) (stateReviews st)) (unReviewId rid)
    nonempty note
    mapM_ (nonempty . learningText) learnings
    mapM_
      ( \d ->
          nonempty (decisionText d)
            >> person (decisionOwner d)
            >> mapM_
              (\deadline -> when (deadline < now) (Left (InvalidInput "결정 기한은 회고 시각 이후여야 합니다.")))
              (decisionDeadline d)
      )
      decisions
    pure
      ( ReviewHeld
          ( Review
              rid
              gid
              (latestResult (resultsOf st gid))
              (evaluateGoal now g (resultsOf st gid))
              learnings
              decisions
              now
              note
          )
      )
  ChangeStrategy gid note -> goal gid >> nonempty note >> pure (StrategyChanged gid note)
  where
    organization = maybe (Left NoOrganization) (const (Right ())) (stateOrganization st)
    person = requireActivePerson st
    goal gid = maybe (Left (GoalNotFound gid)) Right (Map.lookup gid (stateGoals st))
    active gid = unless (Set.member gid (stateActive st)) (Left (GoalNotActive gid))
    duplicate exists ident = when exists (Left (DuplicateId ident))
    nonempty t =
      when
        (T.null (T.strip t) || T.length t > 10000)
        (Left (InvalidInput "텍스트는 1~10000자여야 합니다."))
    identifier t =
      when
        ( T.null t
            || T.length t > 100
            || T.any
              ( \c ->
                  not
                    ( c >= 'a' && c <= 'z'
                        || c >= 'A' && c <= 'Z'
                        || c >= '0' && c <= '9'
                        || c == '-'
                        || c == '_'
                    )
              )
              t
        )
        (Left (InvalidInput "식별자는 영문·숫자·하이픈·밑줄 1~100자여야 합니다."))

checkVersion :: OrgState -> Int -> Either OrganizationError ()
checkVersion st expected =
  unless
    (expected == stateLastSeq st)
    (Left (VersionConflict expected (stateLastSeq st)))

validateProfile :: EmployeeProfile -> Either OrganizationError EmployeeProfile
validateProfile EmployeeProfile {..} = do
  let clean =
        ( >>=
            \value -> let trimmed = T.strip value in if T.null trimmed then Nothing else Just trimmed
        )
      department = clean profileDepartment
      email = clean profileEmail
  mapM_
    (\value -> when (T.length value > 200) (Left (InvalidInput "부서는 200자 이하여야 합니다.")))
    department
  mapM_
    ( \value ->
        when
          ( T.length value > 254
              || T.any (`elem` [' ', '\t', '\n', '\r']) value
              || not (validEmail value)
          )
          (Left (InvalidInput "이메일 형식을 확인해주세요."))
    )
    email
  pure (EmployeeProfile department email)
  where
    validEmail value = case T.splitOn "@" value of
      [local, domain] -> not (T.null local || T.null domain) && T.isInfixOf "." domain
      _               -> False

validateReports :: Map.Map UserId Person -> Either OrganizationError ()
validateReports people = mapM_ (walk Set.empty) (Map.keys people)
  where
    walk seen uid
      | Set.member uid seen = Left (InvalidInput "보고 관계가 자기 자신 또는 순환을 가리킬 수 없습니다.")
      | otherwise = case Map.lookup uid people >>= personReportsTo of
          Nothing   -> Right ()
          Just next -> walk (Set.insert uid seen) next
