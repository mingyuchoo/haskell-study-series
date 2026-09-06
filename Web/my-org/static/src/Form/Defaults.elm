module Form.Defaults exposing (Context, defaultValue, draftDefaults)

import Dict exposing (Dict)
import Domain exposing (Workspace)
import Domain.Permission exposing (permissionKeys)
import Form.Action exposing (..)


type alias Context =
    { seed : String, today : String, deadline : String, goalIndex : Int, workspace : Maybe Workspace }


draftDefaults : Context -> Action -> Dict String String
draftDefaults model action =
    ([ "name", "role", "department", "email", "reportsTo", "successor", "budget", "owner", "reportedBy", "note", "value" ] ++ permissionKeys)
        |> List.map (\key -> ( key, defaultValue model action key ))
        |> Dict.fromList


defaultValue : Context -> Action -> String -> String
defaultValue model action name =
    let
        w =
            model.workspace

        owner key =
            w |> Maybe.andThen (\data -> List.filter (.goal >> .id >> (==) key) data.goals |> List.head |> Maybe.andThen .owner) |> Maybe.withDefault ""
    in
    case action of
        Rename ->
            if name == "name" then
                w |> Maybe.map (.organization >> .name) |> Maybe.withDefault ""

            else
                ""

        UpdatePerson key ->
            let
                person =
                    w |> Maybe.andThen (\data -> List.filter (.id >> (==) key) data.people |> List.head)
            in
            person
                |> Maybe.map
                    (\p ->
                        case name of
                            "name" ->
                                p.name

                            "role" ->
                                p.role

                            "department" ->
                                Maybe.withDefault "" p.department

                            "email" ->
                                Maybe.withDefault "" p.email

                            "reportsTo" ->
                                Maybe.withDefault "" p.reportsTo

                            _ ->
                                ""
                    )
                |> Maybe.withDefault ""

        AddGoal ->
            case name of
                "baseline" ->
                    "0"

                "target" ->
                    "100"

                "budget" ->
                    "0"

                "metricId" ->
                    "metric-" ++ model.seed ++ "-" ++ String.fromInt model.goalIndex

                "direction" ->
                    "HigherIsBetter"

                "startsAt" ->
                    model.today

                "deadline" ->
                    model.deadline

                _ ->
                    ""

        Assign key ->
            if name == "owner" then
                owner key

            else
                ""

        Report key ->
            if name == "reportedBy" then
                owner key

            else
                ""

        Grant key ->
            let
                authority =
                    w |> Maybe.andThen (\data -> List.filter (.owner >> (==) key) data.authorities |> List.head)
            in
            if name == "budget" then
                authority |> Maybe.map (.budgetLimit >> String.fromFloat) |> Maybe.withDefault "0"

            else if authority |> Maybe.map (\a -> List.member name a.canApprove || (name == "Hiring" && a.canHire) || (name == "Pricing" && a.canChangePrice)) |> Maybe.withDefault False then
                "true"

            else
                "false"

        _ ->
            ""



-- Validate at the command boundary as well as with native form constraints.
