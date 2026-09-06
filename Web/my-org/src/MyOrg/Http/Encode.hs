-- | The public HTTP response shape; query logic never builds JSON.
module MyOrg.Http.Encode
  ( encodeQueryResult
  ) where

import Data.Aeson (Value, object)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import MyOrg.Application.ReadModel
import MyOrg.Domain.Event.Types
import MyOrg.Domain.Organization
import MyOrg.Domain.Review (describeReviewWarning)
import MyOrg.Presentation.Event
import MyOrg.Serialization.JSON (toWire, (.=))

encodeQueryResult :: QueryResult -> Value
encodeQueryResult = \case
  OrganizationsResult summaries -> toWire (map summaryJSON summaries)
  SummaryResult summary -> summaryJSON summary
  DashboardResult dashboard -> dashboardJSON dashboard
  OrganizationResult organization -> toWire organization
  PeopleResult people -> toWire (map personJSON people)
  PersonResult person version goals -> object ["person" .= personJSON person, "version" .= version, "ownedGoals" .= goals]
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
    , "people" .= map personJSON dashboardPeople
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

personJSON :: PersonView -> Value
personJSON PersonView {viewPerson = Person {..}, viewProfile = EmployeeProfile {..}, ..} =
  object
    ( catMaybes
        [ Just ("id" .= personId)
        , Just ("name" .= personName)
        , Just ("role" .= personRole)
        , ("reportsTo" .=) <$> personReportsTo
        , ("department" .=) <$> profileDepartment
        , ("email" .=) <$> profileEmail
        , if viewPersonActive then Nothing else Just ("status" .= ("inactive" :: Text))
        ]
    )
