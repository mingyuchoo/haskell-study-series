module MyOrg.Application.Command.Validation
  ( requireOrganization
  , requireGoal
  , requireActiveGoal
  , duplicate
  , nonempty
  , identifier
  , checkVersion
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import MyOrg.Domain.Error
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.State

requireOrganization :: OrgState -> Either OrganizationError Organization
requireOrganization st = maybe (Left NoOrganization) Right (stateOrganization st)

requireGoal :: OrgState -> GoalId -> Either OrganizationError Goal
requireGoal st gid = maybe (Left (GoalNotFound gid)) Right (Map.lookup gid (stateGoals st))

requireActiveGoal :: OrgState -> GoalId -> Either OrganizationError ()
requireActiveGoal st gid = unless (Set.member gid (stateActive st)) (Left (GoalNotActive gid))

duplicate :: Bool -> Text -> Either OrganizationError ()
nonempty :: Text -> Either OrganizationError ()
identifier :: Text -> Either OrganizationError ()
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
