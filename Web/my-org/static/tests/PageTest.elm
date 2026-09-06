module PageTest exposing (tests)

import Dict
import Expect
import Form.Action exposing (Action)
import Form.Goal as Goal
import Form.Review as Review
import Html.Attributes as Attr
import Page.Goals
import Page.Learning
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)


type Msg
    = GoalEdited Goal.Field String
    | ReviewEdited Review.Field String
    | Other


forms =
    { busy = False, fresh = True, saving = Nothing, value = \_ _ -> "", edit = \_ _ _ -> Other, submit = \_ -> Other }


tests : Test
tests =
    describe "Page fields emit typed edits"
        [ test "goal target has a typed field callback" <|
            \_ ->
                Page.Goals.view
                    { forms = forms, expandedGoal = Nothing, results = \_ -> Other, draft = Goal.fromValues (always ""), edit = GoalEdited }
                    { goals = [], people = [], reviews = [], compiler = { errors = 0, warnings = 0, diagnostics = [] } }
                    |> Query.fromHtml
                    |> Query.find [ tag "input", attribute (Attr.name "target") ]
                    |> Event.simulate (Event.input "42")
                    |> Event.expect (GoalEdited Goal.Target "42")
        , test "learning textarea has a typed review callback" <|
            \_ ->
                Page.Learning.view
                    { forms = forms, draft = Review.fromValues (always ""), edit = ReviewEdited }
                    { goals = [], people = [], reviews = [], reviewWarnings = [], events = [] }
                    |> Query.fromHtml
                    |> Query.find [ tag "textarea" ]
                    |> Event.simulate (Event.input "Keep this")
                    |> Event.expect (ReviewEdited Review.Learning "Keep this")
        ]
