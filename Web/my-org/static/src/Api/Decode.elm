module Api.Decode exposing (..)

import Domain exposing (..)
import Json.Decode as D exposing (Decoder)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    D.map2 (|>)


field : String -> Decoder a -> Decoder (a -> b) -> Decoder b
field name decoder =
    andMap (D.field name decoder)


optional : String -> Decoder a -> Decoder (Maybe a)
optional name decoder =
    D.oneOf
        [ D.field name (D.nullable decoder)
        , D.keyValuePairs D.value
            |> D.andThen
                (\fields ->
                    if List.any (Tuple.first >> (==) name) fields then
                        D.fail ("잘못된 필드 형식: " ++ name)

                    else
                        D.succeed Nothing
                )
        ]


statusDecoder : Decoder Status
statusDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "NoData" ->
                        D.succeed NoData

                    "OnTrack" ->
                        D.succeed OnTrack

                    "AtRisk" ->
                        D.succeed AtRisk

                    "OffTrack" ->
                        D.succeed OffTrack

                    "Achieved" ->
                        D.succeed Achieved

                    _ ->
                        D.fail ("알 수 없는 성과 상태: " ++ s)
            )


organizationDecoder : Decoder Organization
organizationDecoder =
    D.map3 Organization (D.field "id" D.string) (D.field "name" D.string) (D.field "createdAt" D.string)


summaryDecoder : Decoder Summary
summaryDecoder =
    D.succeed Summary |> field "organization" organizationDecoder |> field "demo" D.bool |> field "peopleCount" D.int |> field "goalCount" D.int


personDecoder : Decoder Person
personDecoder =
    D.succeed Person
        |> field "id" D.string
        |> field "name" D.string
        |> field "role" D.string
        |> andMap (optional "reportsTo" D.string)
        |> andMap (optional "department" D.string)
        |> andMap (optional "email" D.string)
        |> andMap
            (optional "status" D.string
                |> D.andThen
                    (\status ->
                        case status of
                            Nothing ->
                                D.succeed True

                            Just "active" ->
                                D.succeed True

                            Just "inactive" ->
                                D.succeed False

                            _ ->
                                D.fail "알 수 없는 재직 상태"
                    )
            )


metricDecoder : Decoder Metric
metricDecoder =
    D.succeed Metric |> field "id" D.string |> field "name" D.string |> field "unit" D.string |> field "direction" D.string


goalDecoder : Decoder Goal
goalDecoder =
    D.succeed Goal |> field "id" D.string |> field "description" D.string |> field "metric" metricDecoder |> field "baseline" D.float |> field "target" D.float |> field "deadline" D.string |> field "requiredBudget" D.float |> field "requiredPermissions" (D.list D.string)


evaluationDecoder : Decoder Evaluation
evaluationDecoder =
    D.succeed Evaluation |> field "status" statusDecoder |> field "progress" D.float |> andMap (optional "latestValue" D.float)


analysisDecoder : Decoder Analysis
analysisDecoder =
    D.succeed Analysis |> field "coverage" D.float |> field "possibleCause" D.string


measurementDecoder : Decoder Measurement
measurementDecoder =
    D.succeed Measurement |> field "value" D.float |> field "reportedAt" D.string |> field "note" D.string |> andMap (optional "reportedBy" D.string)


goalViewDecoder : Decoder GoalView
goalViewDecoder =
    D.succeed GoalView |> field "goal" goalDecoder |> andMap (optional "owner" D.string) |> field "active" D.bool |> field "evaluation" evaluationDecoder |> field "analysis" (D.oneOf [ analysisDecoder, D.null (Analysis 0 "구조 분석 대기") ]) |> field "results" (D.list measurementDecoder) |> field "strategies" (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.string)))


authorityDecoder : Decoder Authority
authorityDecoder =
    D.succeed Authority |> field "owner" D.string |> field "budgetLimit" D.float |> field "canHire" D.bool |> field "canChangePrice" D.bool |> field "canApprove" (D.list D.string)


decisionDecoder : Decoder Decision
decisionDecoder =
    D.succeed Decision |> field "text" D.string |> field "owner" D.string |> andMap (optional "deadline" D.string)


reviewDecoder : Decoder Review
reviewDecoder =
    D.succeed Review |> field "id" D.string |> field "goal" D.string |> field "heldAt" D.string |> field "note" D.string |> field "evaluation" evaluationDecoder |> field "learnings" (D.list (D.field "text" D.string)) |> field "decisions" (D.list decisionDecoder)


diagnosticDecoder : Decoder Diagnostic
diagnosticDecoder =
    D.succeed Diagnostic |> field "severity" D.string |> field "code" D.string |> field "message" D.string |> field "subject" D.string |> field "details" (D.list D.string)


compilerDecoder : Decoder Compiler
compilerDecoder =
    D.succeed Compiler |> field "errors" D.int |> field "warnings" D.int |> field "diagnostics" (D.list diagnosticDecoder)


nodeDecoder : Decoder Node
nodeDecoder =
    D.succeed Node |> field "tag" D.string |> field "contents" D.string


edgeDecoder : Decoder Edge
edgeDecoder =
    D.succeed Edge |> field "from" nodeDecoder |> field "to" nodeDecoder |> field "kind" D.string


auditDecoder : Decoder Audit
auditDecoder =
    D.succeed Audit
        |> andMap (D.at [ "record", "seq" ] D.int)
        |> andMap (D.at [ "record", "at" ] D.string)
        |> andMap (D.field "record" (optional "actor" D.string))
        |> field "description" D.string
        |> andMap
            (D.oneOf
                [ D.at [ "record", "event" ]
                    (D.field "tag" D.string
                        |> D.andThen
                            (\tag ->
                                if tag == "GoalEvaluated" then
                                    D.field "contents" (D.index 0 D.string |> D.map Just)

                                else
                                    D.succeed Nothing
                            )
                    )
                , D.succeed Nothing
                ]
            )
        |> andMap (D.oneOf [ D.at [ "record", "event", "contents" ] (D.index 1 (D.field "status" statusDecoder)) |> D.map Just, D.succeed Nothing ])


workspaceDecoder : Decoder Workspace
workspaceDecoder =
    D.succeed Workspace |> field "organization" organizationDecoder |> field "version" D.int |> field "demo" D.bool |> field "people" (D.list personDecoder) |> field "goals" (D.list goalViewDecoder) |> field "authorities" (D.list authorityDecoder) |> field "reviews" (D.list reviewDecoder) |> field "compiler" compilerDecoder |> andMap (D.at [ "graph", "edges" ] (D.list edgeDecoder)) |> field "events" (D.list auditDecoder) |> field "decisionShare" (D.dict D.float) |> field "reviewWarnings" (D.list (D.map2 ReviewWarning (D.field "id" D.string) (D.field "warnings" (D.list D.string))))
