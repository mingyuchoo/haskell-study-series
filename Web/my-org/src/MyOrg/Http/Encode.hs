-- | The public HTTP response shape; query logic never builds JSON.
module MyOrg.Http.Encode
  ( encodeQueryResult
  ) where

import Data.Aeson (Value, object)
import MyOrg.Application.ReadModel
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Review (describeReviewWarning)
import MyOrg.Presentation.Event
import MyOrg.Serialization.JSON (toWire, (.=))

encodeQueryResult :: QueryResult -> Value
encodeQueryResult = \case
  OrganizationsResult summaries -> toWire (map summaryJSON summaries)
  SummaryResult summary -> summaryJSON summary
  DashboardResult dashboard -> dashboardJSON dashboard
  OrganizationResult organization -> toWire organization
  PeopleResult people -> toWire people
  GoalsResult goals -> toWire (map goalJSON goals)
  CompilerResult report -> toWire report
  GraphResult graph -> toWire graph
  EventsResult events -> toWire events
  ReviewsResult reviews -> toWire reviews

summaryJSON :: OrganizationSummary -> Value
summaryJSON OrganizationSummary {..} =
  object
    [ "organization" .= summaryOrganization
    , "version" .= summaryVersion
    , "demo" .= summaryDemo
    , "peopleCount" .= summaryPeopleCount
    , "goalCount" .= summaryGoalCount
    ]

goalJSON :: GoalView -> Value
goalJSON GoalView {..} =
  object
    [ "goal" .= viewGoal
    , "owner" .= viewOwner
    , "active" .= viewActive
    , "evaluation" .= viewEvaluation
    , "analysis" .= viewAnalysis
    , "results" .= viewResults
    , "strategies" .= viewStrategies
    ]

dashboardJSON :: Dashboard -> Value
dashboardJSON Dashboard {..} =
  object
    [ "version" .= dashboardVersion
    , "demo" .= dashboardDemo
    , "organization" .= dashboardOrganization
    , "people" .= dashboardPeople
    , "goals" .= map goalJSON dashboardGoals
    , "authorities" .= dashboardAuthorities
    , "compiler" .= dashboardCompiler
    , "graph" .= dashboardGraph
    , "decisionShare" .= dashboardDecisionShare
    , "reviews" .= dashboardReviews
    , "reviewWarnings"
        .= [ object ["id" .= rid, "warnings" .= map describeReviewWarning warnings]
           | (rid, warnings) <- dashboardReviewWarnings
           ]
    , "events"
        .= [ object ["record" .= event, "description" .= describeEvent (storedEvent event)]
           | event <- dashboardEvents
           ]
    ]
