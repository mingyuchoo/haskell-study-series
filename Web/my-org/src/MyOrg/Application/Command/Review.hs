module MyOrg.Application.Command.Review
  ( reportResult
  , evaluateGoalResult
  , holdReview
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Time (UTCTime)
import MyOrg.Domain.Error
import MyOrg.Domain.Evaluation (evaluateGoal, latestResult)
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Identity
import MyOrg.Domain.Queries
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types
import MyOrg.Domain.State

import MyOrg.Application.Command.Validation

reportResult
  :: UTCTime
  -> OrgState
  -> GoalId
  -> Double
  -> UserId
  -> Text
  -> Either OrganizationError OrganizationEvent
reportResult now st gid value uid note = do
  _ <- requireGoal st gid
  requireActiveGoal st gid
  requireActivePerson st uid
  when (isNaN value || isInfinite value) (Left (InvalidInput "결과는 유한한 숫자여야 합니다."))
  nonempty note
  pure (ResultReported gid (Result gid value now uid note))

evaluateGoalResult
  :: UTCTime -> OrgState -> GoalId -> Either OrganizationError OrganizationEvent
evaluateGoalResult now st gid = do
  g <- requireGoal st gid
  requireActiveGoal st gid
  pure (GoalEvaluated gid (evaluateGoal now g (resultsOf st gid)))

holdReview
  :: UTCTime
  -> OrgState
  -> ReviewId
  -> GoalId
  -> [Learning]
  -> [Decision]
  -> Text
  -> Either OrganizationError OrganizationEvent
holdReview now st rid gid learnings decisions note = do
  g <- requireGoal st gid
  requireActiveGoal st gid
  identifier (unReviewId rid)
  duplicate (any ((== rid) . reviewId) (stateReviews st)) (unReviewId rid)
  nonempty note
  mapM_ (nonempty . learningText) learnings
  mapM_
    ( \d ->
        nonempty (decisionText d)
          >> requireActivePerson st (decisionOwner d)
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
