-- | Projection over multiple independently scoped single-organization engines.
-- Legacy events are assigned by their own creation cursor. Scoped events never
-- move that cursor, so appending new-format events cannot reinterpret old data.
module MyOrg.Registry
  ( Registry (..)
  , replayRegistry
  , activeOrganizations
  , organizationState
  , resolveSingleOrganization
  ) where

import Control.Monad (foldM, unless, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Reducer
import MyOrg.Domain.State

data Registry = Registry
  { registryStates  :: Map OrgId OrgState
  , registryEvents  :: Map OrgId [StoredEvent]
  , registryLastSeq :: Int
  }
  deriving (Show, Eq)

replayRegistry :: [StoredEvent] -> Either OrganizationError Registry
replayRegistry events = fst <$> foldM step (Registry Map.empty Map.empty 0, Nothing) events
  where
    step (registry, legacy) stored = do
      unless
        (storedSeq stored == registryLastSeq registry + 1)
        (Left (InvalidInput "이벤트의 전역 순번이 연속되지 않습니다."))
      (oid, event, nextLegacy) <- case storedEvent stored of
        OrganizationScoped oid event -> pure (oid, event, legacy)
        event@(OrganizationCreated org) -> pure (organizationId org, event, Just (organizationId org))
        event@(OrganizationDeleted oid) -> case legacy of
          Just current | current == oid -> pure (oid, event, Nothing)
          _ -> Left (InvalidInput "구형 삭제 이벤트의 조직 범위가 일치하지 않습니다.")
        event -> case legacy of
          Just oid -> pure (oid, event, legacy)
          Nothing  -> Left (InvalidInput "조직 범위가 없는 이벤트입니다.")
      validateScope oid event
      let st = Map.findWithDefault emptyState oid (registryStates registry)
      case event of
        OrganizationCreated _ -> when (isJust (stateOrganization st)) (Left OrganizationAlreadyExists)
        _ -> unless (isJust (stateOrganization st)) (Left (OrganizationNotFound oid))
      let unwrapped = stored {storedEvent = event}
          next = applyEvent st unwrapped
      pure
        ( registry
            { registryStates = Map.insert oid next (registryStates registry)
            , registryEvents = Map.insertWith (flip (++)) oid [unwrapped] (registryEvents registry)
            , registryLastSeq = max (registryLastSeq registry) (storedSeq stored)
            }
        , nextLegacy
        )

validateScope :: OrgId -> OrganizationEvent -> Either OrganizationError ()
validateScope oid event = case event of
  OrganizationScoped _ _      -> Left (InvalidInput "중첩된 조직 범위 이벤트는 허용하지 않습니다.")
  OrganizationCreated org     -> matches (organizationId org)
  OrganizationDeleted inner   -> matches inner
  OrganizationRenamed inner _ -> matches inner
  DemoSeeded inner            -> matches inner
  GoalCreated goal            -> matches (goalOrganization goal)
  _                           -> Right ()
  where
    matches inner = unless (oid == inner) (Left (InvalidInput "이벤트 내부 조직과 저장 범위가 일치하지 않습니다."))

activeOrganizations :: Registry -> [OrgState]
activeOrganizations = filter (isJust . stateOrganization) . Map.elems . registryStates

organizationState :: Registry -> OrgId -> Either OrganizationError OrgState
organizationState registry oid = case Map.lookup oid (registryStates registry) of
  Just st | isJust (stateOrganization st) -> Right st
  _                                       -> Left (OrganizationNotFound oid)

resolveSingleOrganization :: Registry -> Either OrganizationError (Maybe OrgId)
resolveSingleOrganization registry = case activeOrganizations registry of
  []   -> Right Nothing
  [st] -> Right (organizationId <$> stateOrganization st)
  _    -> Left AmbiguousOrganizations
