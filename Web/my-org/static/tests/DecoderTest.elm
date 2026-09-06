module DecoderTest exposing (tests)

import Api.Decode exposing (..)
import Domain exposing (..)
import Expect
import Json.Decode as D
import Test exposing (..)


tests : Test
tests =
    describe "HTTP JSON contract"
        [ test "legacy person defaults to active without optional profile" <|
            \_ ->
                D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role"}"""
                    |> Expect.equal (Ok { id = "p", name = "Name", role = "Role", reportsTo = Nothing, department = Nothing, email = Nothing, active = True })
        , test "inactive person retains profile and report link" <|
            \_ ->
                D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role","reportsTo":"boss","department":"Team","email":"a@example.com","status":"inactive"}"""
                    |> Expect.equal (Ok { id = "p", name = "Name", role = "Role", reportsTo = Just "boss", department = Just "Team", email = Just "a@example.com", active = False })
        , test "unknown employment status and malformed profile are rejected" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.err (D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role","status":"unknown"}""")
                    , \_ -> Expect.err (D.decodeString personDecoder """{"id":"p","name":"Name","role":"Role","email":42}""")
                    ]
                    ()
        , test "optional measurement is absent or null, never silently mistyped" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (Ok { status = NoData, progress = 0, latestValue = Nothing }) (D.decodeString evaluationDecoder "{\"status\":\"NoData\",\"progress\":0}")
                    , \_ -> Expect.equal (Ok Nothing) (D.decodeString (optional "latestValue" D.float) "{\"latestValue\":null}")
                    , \_ -> Expect.err (D.decodeString evaluationDecoder "{\"status\":\"NoData\",\"progress\":0,\"latestValue\":\"bad\"}")
                    ]
                    ()
        , test "unknown performance states are rejected" <|
            \_ -> Expect.err (D.decodeString statusDecoder "\"FutureState\"")
        , test "all known performance states decode" <|
            \_ -> List.map (\s -> D.decodeString statusDecoder ("\"" ++ s ++ "\"")) [ "NoData", "OnTrack", "AtRisk", "OffTrack", "Achieved" ] |> Expect.equal (List.map Ok [ NoData, OnTrack, AtRisk, OffTrack, Achieved ])
        , test "summary requires counts and organization metadata" <|
            \_ -> Expect.err (D.decodeString summaryDecoder "{\"organization\":{\"id\":\"o\",\"name\":\"A\",\"createdAt\":\"now\"},\"demo\":false,\"peopleCount\":0}")
        , test "workspace follows nested graph and compiler response contract" <|
            \_ ->
                D.decodeString workspaceDecoder """{"organization":{"id":"o","name":"A","createdAt":"now"},"version":2,"demo":false,"people":[],"goals":[],"authorities":[],"reviews":[],"compiler":{"errors":0,"warnings":0,"diagnostics":[]},"graph":{"edges":[]},"events":[],"decisionShare":{},"reviewWarnings":[]}"""
                    |> Result.map (\w -> ( w.organization.id, w.version, w.edges ))
                    |> Expect.equal (Ok ( "o", 2, [] ))
        , test "goal evaluation audit uses tagged event tuple" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":3,"at":"now","actor":null,"event":{"tag":"GoalEvaluated","contents":["goal-1",{"status":"OnTrack"}]}},"description":"evaluated"}"""
                    |> Result.map (\a -> ( a.evaluatedGoal, a.evaluatedStatus ))
                    |> Expect.equal (Ok ( Just "goal-1", Just OnTrack ))
        , test "review learning objects and optional decision deadline decode" <|
            \_ ->
                D.decodeString reviewDecoder """{"id":"r","goal":"g","heldAt":"now","note":"review","evaluation":{"status":"NoData","progress":0},"learnings":[{"text":"learned"}],"decisions":[{"text":"next","owner":"p"}]}"""
                    |> Result.map (\r -> ( r.learnings, r.decisions ))
                    |> Expect.equal (Ok ( [ "learned" ], [ { text = "next", owner = "p", deadline = Nothing } ] ))
        , test "scoped owner assignment retains structural IDs independent of description" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":4,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"OrganizationScoped","contents":["org-a",{"tag":"OwnerAssigned","contents":["goal-a","person-a"]}]}},"description":"arbitrary prose"}"""
                    |> Result.map (\a -> ( a.activity.tag, a.activity.targetId, a.activity.personId ))
                    |> Expect.equal (Ok ( "OwnerAssigned", "goal-a", Just "person-a" ))
        , test "review event retains both goal and exact review identity" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":5,"at":"2026-09-07T00:00:00Z","actor":"person-a","event":{"tag":"ReviewHeld","contents":{"id":"review-a","goal":"goal-a","note":"회고"}}},"description":"review"}"""
                    |> Result.map (\a -> ( a.activity.targetId, a.activity.reviewId ))
                    |> Expect.equal (Ok ( "goal-a", Just "review-a" ))
        , test "unknown event preserves raw payload for inspection" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":6,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"FutureEvent","contents":{"futureField":"kept"}}},"description":"future"}"""
                    |> Result.map (\a -> ( a.activity.tag, String.contains "futureField" a.activity.raw ))
                    |> Expect.equal (Ok ( "FutureEvent", True ))
        , test "employee profile events extract person ID from tuple rather than prose" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":7,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"PersonUpdated","contents":[{"id":"person-a","name":"새 이름","role":"개발"},{"department":"제품"}]}},"description":"old name person-b"}"""
                    |> Result.map (\a -> ( a.activity.targetKind, a.activity.targetId ))
                    |> Expect.equal (Ok ( "person", "person-a" ))
        , test "scoped evaluation continues to drive guide progress" <|
            \_ ->
                D.decodeString auditDecoder """{"record":{"seq":8,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"OrganizationScoped","contents":["org-a",{"tag":"GoalEvaluated","contents":["demo-revenue",{"status":"Achieved"}]}]}},"description":"evaluated"}"""
                    |> Result.map (\a -> ( a.evaluatedGoal, a.evaluatedStatus ))
                    |> Expect.equal (Ok ( Just "demo-revenue", Just Achieved ))
        ]
