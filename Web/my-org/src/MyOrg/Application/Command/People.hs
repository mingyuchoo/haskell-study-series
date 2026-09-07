module MyOrg.Application.Command.People
  ( addPerson
  , addEmployee
  , updatePerson
  , deactivatePerson
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import MyOrg.Domain.Authority
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Queries
import MyOrg.Domain.State

import MyOrg.Application.Command.Validation

addPerson :: OrgState -> Person -> Either OrganizationError OrganizationEvent
addPerson st p = validateNewPerson st p >> pure (PersonAdded p)

validateNewPerson :: OrgState -> Person -> Either OrganizationError ()
validateNewPerson st p = do
  _ <- requireOrganization st
  identifier (unUserId (personId p))
  nonempty (personName p)
  nonempty (personRole p)
  duplicate (Map.member (personId p) (statePeople st)) (unUserId (personId p))
  mapM_ (requireActivePerson st) (personReportsTo p)

addEmployee
  :: OrgState -> Person -> EmployeeProfile -> Either OrganizationError OrganizationEvent
addEmployee st p profile = do
  validateNewPerson st p
  cleaned <- validateProfile profile
  pure (if cleaned == emptyProfile then PersonAdded p else EmployeeAdded p cleaned)

updatePerson
  :: OrgState -> Person -> EmployeeProfile -> Int -> Either OrganizationError OrganizationEvent
updatePerson st p profile version = do
  checkVersion st version
  unless (Map.member (personId p) (statePeople st)) (Left (PersonNotFound (personId p)))
  nonempty (personName p)
  nonempty (personRole p)
  mapM_ (requireActivePerson st) (personReportsTo p)
  validateReports (Map.insert (personId p) p (statePeople st))
  cleaned <- validateProfile profile
  pure (PersonUpdated p cleaned)

deactivatePerson
  :: OrgState -> UserId -> Maybe UserId -> Int -> Either OrganizationError OrganizationEvent
deactivatePerson st uid successor version = do
  checkVersion st version
  requireActivePerson st uid
  let departing = statePeople st Map.! uid
      owns = any ((== uid) . ownershipOwner) (Map.elems (stateOwnership st))
      reports = any ((== Just uid) . personReportsTo) (Map.elems (statePeople st))
  when
    ((owns || reports) && successor == Nothing)
    (Left (InvalidInput "담당 목표 또는 직속 보고자가 있어 활성 인계 대상이 필요합니다."))
  mapM_
    ( \next -> do
        when (next == uid) (Left (InvalidInput "자기 자신에게 인계할 수 없습니다."))
        requireActivePerson st next
        validateReports (Map.map (handoverReports departing next) (statePeople st))
    )
    successor
  pure (PersonDeactivated uid successor)

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
