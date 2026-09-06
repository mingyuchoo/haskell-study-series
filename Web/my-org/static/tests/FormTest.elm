module FormTest exposing (tests)

import Expect
import Form.Goal as Goal
import Form.Review as Review
import Test exposing (..)


goal : Goal.Draft
goal =
    { description = "Revenue", metricName = "Sales", unit = "KRW", metricId = "sales", direction = "HigherIsBetter", baseline = "0", target = "100", startsAt = "2026-01-01", deadline = "2026-12-31", budget = "12.5", parent = "", permissions = [ "Pricing" ] }


review : Review.Draft
review =
    { goal = "goal-a", note = "Reflection", learning = "Learning", decision = "", decisionOwner = "", decisionDeadline = "" }


tests : Test
tests =
    describe "Typed drafts validate at submission boundary"
        [ test "goal converts numeric text only on submission" <|
            \_ -> Goal.validate goal |> Result.map (\g -> ( g.baseline, g.target, g.budget )) |> Expect.equal (Ok ( 0, 100, 12.5 ))
        , test "incomplete number remains editable and cannot submit" <|
            \_ ->
                let
                    edited =
                        Goal.edit Goal.Target "-" goal
                in
                Expect.all [ \_ -> Expect.equal "-" edited.target, \_ -> Expect.err (Goal.validate edited) ] ()
        , test "goal rejects missing description" <|
            \_ -> Expect.err (Goal.validate { goal | description = "  " })
        , test "goal rejects deadline before start" <|
            \_ -> Expect.err (Goal.validate { goal | deadline = "2025-12-31" })
        , test "permission edits preserve other draft fields" <|
            \_ -> Goal.edit (Goal.Permission "Hiring") "true" goal |> Goal.edit (Goal.Permission "Pricing") "false" |> (\g -> ( g.permissions, g.description, g.budget )) |> Expect.equal ( [ "Hiring" ], "Revenue", "12.5" )
        , test "learning without a decision is valid" <|
            \_ -> Expect.equal (Ok review) (Review.validate review)
        , test "decision requires its owner" <|
            \_ -> Expect.err (Review.validate { review | decision = "Launch" })
        , test "decision can omit its deadline" <|
            \_ -> Review.validate { review | decision = "Launch", decisionOwner = "owner" } |> Result.map .decisionDeadline |> Expect.equal (Ok "")
        ]
