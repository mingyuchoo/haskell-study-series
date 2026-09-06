module Api.Command exposing (Context, payload)

import Api.Path exposing (orgPath)
import Domain.Permission exposing (permissionKeys)
import Form.Action exposing (..)
import Form.Goal
import Form.Review
import Form.Snapshot exposing (Snapshot)
import Json.Encode as E
import Url


type alias Context =
    { org : Maybe String, seed : String, serial : Int, version : Int, deletion : Maybe Snapshot, value : Action -> String -> String, goal : Form.Goal.Draft, review : Form.Review.Draft }


payload : Context -> Action -> Result String ( String, String, E.Value )
payload model action =
    let
        val =
            model.value action

        str key =
            ( key, E.string (val key) )

        uid prefix =
            E.string (prefix ++ "-" ++ model.seed ++ "-" ++ String.fromInt model.serial)

        num key =
            E.float (String.toFloat (val key) |> Maybe.withDefault 0)

        nullable value_ =
            if value_ == "" then
                E.null

            else
                E.string value_

        ps =
            E.list E.string (permissionKeys |> List.filter (\key -> val key == "true"))

        path tail =
            orgPath (Maybe.withDefault "" model.org) tail

        post route fields =
            Ok ( "POST", route, E.object fields )

        version =
            model.value action "__version" |> String.toInt |> Maybe.withDefault model.version

        profile =
            [ str "name", str "role", ( "department", nullable (String.trim (val "department")) ), ( "email", nullable (String.trim (val "email")) ), ( "reportsTo", nullable (val "reportsTo") ) ]

        current result =
            if version /= model.version then
                Err "작성 중 조직이 변경되었습니다. ‘최신 정보로 다시 불러오기’를 눌러 변경 내용을 확인한 뒤 다시 작성해 주세요."

            else
                result

        blank keys =
            List.any (\key -> String.trim (val key) == "") keys

        badNumber keys =
            List.any (\key -> String.toFloat (val key) == Nothing) keys

        validate keys nums result =
            if blank keys then
                Err "필수 항목을 모두 입력하세요."

            else if badNumber nums then
                Err "숫자 항목을 올바르게 입력하세요."

            else
                result
    in
    case action of
        CreateOrg ->
            validate [ "name" ] [] (post "/api/organizations" [ ( "id", uid "org" ), str "name" ])

        ImportDemo ->
            post "/api/demo" []

        Rename ->
            validate [ "name" ]
                []
                (if version /= model.version then
                    Err "작성 중 조직이 변경되었습니다. 최신 조직 이름을 확인하고 수정 입력을 다시 해 주세요."

                 else
                    Ok ( "PATCH", path "", E.object [ str "name", ( "expectedVersion", E.int version ) ] )
                )

        AddPerson ->
            validate [ "name", "role" ] [] (post (path "people") (( "id", uid "person" ) :: profile))

        UpdatePerson key ->
            validate [ "name", "role" ]
                []
                (current (Ok ( "PATCH", path ("people/" ++ Url.percentEncode key), E.object (( "expectedVersion", E.int version ) :: profile) )))

        DeactivatePerson key ->
            current (post (path ("people/" ++ Url.percentEncode key ++ "/deactivate")) [ ( "successor", nullable (val "successor") ), ( "expectedVersion", E.int version ) ])

        AddGoal ->
            Form.Goal.validate model.goal
                |> Result.map
                    (\goal ->
                        ( "POST"
                        , path "goals"
                        , E.object
                            [ ( "id", uid "goal" )
                            , ( "organization", E.string (Maybe.withDefault "" model.org) )
                            , ( "description", E.string goal.description )
                            , ( "metric", E.object [ ( "id", E.string goal.metricId ), ( "name", E.string goal.metricName ), ( "unit", E.string goal.unit ), ( "direction", E.string goal.direction ) ] )
                            , ( "baseline", E.float goal.baseline )
                            , ( "target", E.float goal.target )
                            , ( "startsAt", E.string (goal.startsAt ++ "T00:00:00Z") )
                            , ( "deadline", E.string (goal.deadline ++ "T00:00:00Z") )
                            , ( "parent", nullable goal.parent )
                            , ( "requiredPermissions", E.list E.string (permissionKeys |> List.filter (\key -> List.member key goal.permissions)) )
                            , ( "requiredBudget", E.float goal.budget )
                            ]
                        )
                    )

        Assign key ->
            validate [ "owner" ] [] (post (path ("goals/" ++ Url.percentEncode key ++ "/owner")) [ str "owner" ])

        Grant key ->
            validate [] [ "budget" ] (post (path ("people/" ++ Url.percentEncode key ++ "/authority")) [ ( "owner", E.string key ), ( "budgetLimit", num "budget" ), ( "canHire", E.bool False ), ( "canChangePrice", E.bool False ), ( "canApprove", ps ) ])

        Report key ->
            validate [ "reportedBy", "note" ] [ "value" ] (post (path ("goals/" ++ Url.percentEncode key ++ "/results")) [ ( "value", num "value" ), str "reportedBy", str "note", ( "actor", E.string (val "reportedBy") ) ])

        Strategy key ->
            validate [ "note" ] [] (post (path ("goals/" ++ Url.percentEncode key ++ "/strategy")) [ str "note" ])

        Activate key ->
            post (path ("goals/" ++ Url.percentEncode key ++ "/activate")) []

        Evaluate key ->
            post (path "evaluations") [ ( "goal", E.string key ) ]

        AddReview ->
            Form.Review.validate model.review
                |> Result.map
                    (\review ->
                        ( "POST"
                        , path "reviews"
                        , E.object
                            [ ( "id", uid "review" )
                            , ( "goal", E.string review.goal )
                            , ( "note", E.string review.note )
                            , ( "learnings"
                              , E.list identity
                                    (if String.trim review.learning == "" then
                                        []

                                     else
                                        [ E.object [ ( "text", E.string review.learning ) ] ]
                                    )
                              )
                            , ( "decisions"
                              , E.list identity
                                    (if String.trim review.decision == "" then
                                        []

                                     else
                                        [ E.object
                                            [ ( "text", E.string review.decision )
                                            , ( "owner", E.string review.decisionOwner )
                                            , ( "deadline"
                                              , if review.decisionDeadline == "" then
                                                    E.null

                                                else
                                                    E.string (review.decisionDeadline ++ "T23:59:59Z")
                                              )
                                            ]
                                        ]
                                    )
                              )
                            ]
                        )
                    )

        DeleteOrg ->
            case model.deletion of
                Just snapshot ->
                    if snapshot.confirmation == snapshot.name && model.org == Just snapshot.id then
                        Ok ( "DELETE", orgPath snapshot.id "", E.object [ ( "confirmName", E.string snapshot.confirmation ), ( "expectedVersion", E.int snapshot.version ) ] )

                    else
                        Err "조직 이름을 정확히 입력하세요."

                Nothing ->
                    Err "삭제 확인을 먼저 열어 주세요."
