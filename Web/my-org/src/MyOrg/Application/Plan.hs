-- | Pure planning against one consistent audit snapshot. No clock or storage IO.
module MyOrg.Application.Plan
  ( planCommand
  , planDemo
  ) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import MyOrg.Application
import MyOrg.Demo (demoEvents, demoOrganizationId)
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Queries (requireActivePerson)
import MyOrg.Domain.State
import MyOrg.Registry

planCommand
  :: UTCTime
  -> [StoredEvent]
  -> Maybe OrgId
  -> Maybe UserId
  -> Command
  -> Either OrganizationError [StoredEvent]
planCommand now events selected actor command = do
  registry <- replayRegistry events
  oid <- commandScope registry selected command
  st <- case command of
    CreateOrganization _ _ -> pure (Map.findWithDefault emptyState oid (registryStates registry))
    _ -> organizationState registry oid
  mapM_ (requireActivePerson st) actor
  changes <- executeCommand now st command
  let additions =
        zipWith
          (\n event -> StoredEvent n now actor (OrganizationScoped oid event))
          [length events + 1 ..]
          changes
  _ <- replayRegistry (events <> additions)
  pure additions

commandScope :: Registry -> Maybe OrgId -> Command -> Either OrganizationError OrgId
commandScope registry selected command = do
  let intrinsic = case command of
        CreateOrganization oid _   -> Just oid
        RenameOrganization oid _ _ -> Just oid
        DeleteOrganization oid _ _ -> Just oid
        _                          -> Nothing
  case (selected, intrinsic) of
    (Just target, Just inner) | target /= inner -> Left (InvalidInput "명령의 조직과 선택한 조직이 일치하지 않습니다.")
    (Just target, _) -> pure target
    (_, Just target) -> pure target
    _ -> resolveSingleOrganization registry >>= maybe (Left NoOrganization) Right

planDemo :: UTCTime -> [StoredEvent] -> Either OrganizationError [StoredEvent]
planDemo now events = do
  registry <- replayRegistry events
  case organizationState registry demoOrganizationId of
    Right _ -> Left OrganizationAlreadyExists
    Left _  -> Right ()
  generated <- demoEvents now
  let additions =
        map
          ( \event ->
              event
                { storedSeq = storedSeq event + length events
                , storedEvent = OrganizationScoped demoOrganizationId (storedEvent event)
                }
          )
          generated
  _ <- replayRegistry (events <> additions)
  pure additions
