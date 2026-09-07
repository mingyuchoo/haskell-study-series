module MyOrg.Application.Command.Discovery
  ( saveDiscovery
  ) where

import Control.Monad (unless, when)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import MyOrg.Application.Command.Validation
import MyOrg.Domain.Discovery
import MyOrg.Domain.Error
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Queries (requireActivePerson)
import MyOrg.Domain.State

saveDiscovery
  :: OrgState -> Discovery -> Int -> Either OrganizationError OrganizationEvent
saveDiscovery st document version = do
  _ <- requireOrganization st
  checkVersion st version
  validateDiscovery document
  validateReferences st document
  let review = discoveryReview document
      needsReview =
        not (sameDiscoveryContent (stateDiscovery st) document)
          || null (discoveryWorkflows document)
      saved =
        if needsReview
          then document {discoveryReview = review {discoveryReviewStatus = Pending}}
          else document
  pure (DiscoverySaved saved)

-- | 참조 필드는 현재 조직의 활성 구성원과 같은 문서의 다른 업무만 가리킬 수 있다.
validateReferences :: OrgState -> Discovery -> Either OrganizationError ()
validateReferences st Discovery {..} = mapM_ check discoveryWorkflows
  where
    ids = Set.fromList (map workflowId discoveryWorkflows)
    check Workflow {..} = do
      mapM_ (requireActivePerson st) workflowRolePerson
      mapM_ (requireActivePerson st) workflowApprovalPerson
      mapM_
        ( \target -> do
            when (target == workflowId) (Left (InvalidInput "업무는 자기 자신에게 인계할 수 없습니다."))
            unless (Set.member target ids) (Left (InvalidInput ("인계 대상 업무를 찾을 수 없습니다: " <> target)))
        )
        workflowHandoffWorkflows

validateDiscovery :: Discovery -> Either OrganizationError ()
validateDiscovery Discovery {..} = do
  mapM_ bounded [discoveryScope, discoveryAsOf, discoveryReviewNote discoveryReview]
  unless
    (T.null discoveryAsOf || validDate discoveryAsOf)
    (Left (InvalidInput "기준 시점은 YYYY-MM-DD 형식의 실제 날짜여야 합니다."))
  unique (map observationId discoveryObservations)
  unique (map workflowId discoveryWorkflows)
  when
    (length discoveryObservations > 200 || length discoveryWorkflows > 200)
    (Left (InvalidInput "현황과 업무는 각각 200개까지 기록할 수 있습니다."))
  mapM_ validateObservation discoveryObservations
  mapM_ validateWorkflow discoveryWorkflows

validDate :: Text -> Bool
validDate value =
  T.length value == 10 && case (parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack value) :: Maybe Day) of
    Just _  -> True
    Nothing -> False

unique :: [Text] -> Either OrganizationError ()
unique ids = do
  mapM_ identifier ids
  unless
    (Set.size (Set.fromList ids) == length ids)
    (Left (InvalidInput "현황 또는 업무의 ID가 중복되었습니다."))

bounded :: Text -> Either OrganizationError ()
bounded value = when (T.length value > 10000) (Left (InvalidInput "입력은 항목당 10000자 이하여야 합니다."))

validateEvidence :: KnowledgeStatus -> Text -> Either OrganizationError ()
validateEvidence status evidence = do
  bounded evidence
  when (status == Confirmed) (nonempty evidence)

validateObservation :: Observation -> Either OrganizationError ()
validateObservation Observation {..} = do
  nonempty observationSubject
  bounded observationDetail
  validateEvidence observationStatus observationEvidence

validateWorkflow :: Workflow -> Either OrganizationError ()
validateWorkflow Workflow {..} = do
  nonempty workflowName
  mapM_
    bounded
    [ workflowRole
    , workflowTrigger
    , workflowInputs
    , workflowTools
    , workflowOutputs
    , workflowHandoff
    , workflowApproval
    ]
  validateEvidence workflowStatus workflowEvidence
