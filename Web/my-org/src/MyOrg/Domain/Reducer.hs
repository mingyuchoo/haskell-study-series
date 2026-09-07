module MyOrg.Domain.Reducer
  ( applyEvent
  , applyEvents
  , replay
  , currentEpoch
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import MyOrg.Domain.Authority
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Organization
import MyOrg.Domain.State
import MyOrg.Domain.Validation

applyEvent :: OrgState -> StoredEvent -> OrgState
applyEvent st StoredEvent {storedSeq, storedAt, storedEvent} =
  let next = (step storedEvent) {stateLastSeq = max (stateLastSeq st) storedSeq}
   in next
        { stateActive =
            Set.filter
              (\gid -> either (const False) (const True) (validateActive next gid))
              (stateActive next)
        }
  where
    step = \case
      OrganizationCreated o -> emptyState {stateOrganization = Just o}
      OrganizationScoped _ event -> step event
      OrganizationRenamed _ name ->
        st
          { stateOrganization = fmap (\org -> org {organizationName = name}) (stateOrganization st)
          }
      OrganizationDeleted _ -> emptyState
      DemoSeeded _ -> st
      DiscoverySaved document -> st {stateDiscovery = document}
      PersonAdded p -> st {statePeople = Map.insert (personId p) p (statePeople st)}
      EmployeeAdded p profile ->
        st
          { statePeople = Map.insert (personId p) p (statePeople st)
          , stateProfiles = Map.insert (personId p) profile (stateProfiles st)
          }
      PersonUpdated p profile ->
        st
          { statePeople = Map.insert (personId p) p (statePeople st)
          , stateProfiles = Map.insert (personId p) profile (stateProfiles st)
          }
      PersonDeactivated uid successor ->
        let affected = Map.keysSet (Map.filter ((== uid) . ownershipOwner) (stateOwnership st))
            people = case (Map.lookup uid (statePeople st), successor) of
              (Just departing, Just next) -> Map.map (handoverReports departing next) (statePeople st)
              _ -> Map.adjust (\p -> p {personReportsTo = Nothing}) uid (statePeople st)
            transfer ownership = case successor of
              Just next
                | ownershipOwner ownership == uid ->
                    ownership {ownershipOwner = next, ownershipSince = storedAt}
              _ -> ownership
         in st
              { statePeople = people
              , stateInactivePeople = Set.insert uid (stateInactivePeople st)
              , stateOwnership = Map.map transfer (stateOwnership st)
              , stateAuthorities = Map.delete uid (stateAuthorities st)
              , stateActive = Set.difference (stateActive st) affected
              }
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

-- | Only the active organization's lifecycle is visible to ordinary API reads.
-- The underlying append-only audit remains intact across deletion/recreation.
currentEpoch :: [StoredEvent] -> [StoredEvent]
currentEpoch = reverse . foldl step []
  where
    step _ event@StoredEvent {storedEvent = OrganizationCreated _} = [event]
    step _ StoredEvent {storedEvent = OrganizationDeleted _}       = []
    step [] _                                                      = []
    step events event                                              = event : events
