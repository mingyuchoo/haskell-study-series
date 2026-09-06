module Domain exposing (..)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)



-- API values become explicit, immutable domain records at the boundary.


type Status
    = NoData
    | OnTrack
    | AtRisk
    | OffTrack
    | Achieved


type alias Organization =
    { id : String, name : String, createdAt : String }


type alias Summary =
    { organization : Organization, demo : Bool, peopleCount : Int, goalCount : Int }


type alias Person =
    { id : String, name : String, role : String }


type alias Metric =
    { id : String, name : String, unit : String, direction : String }


type alias Goal =
    { id : String, description : String, metric : Metric, baseline : Float, target : Float, deadline : String, requiredBudget : Float, requiredPermissions : List String }


type alias Evaluation =
    { status : Status, progress : Float, latestValue : Maybe Float }


type alias Analysis =
    { coverage : Float, possibleCause : String }


type alias Measurement =
    { value : Float, reportedAt : String, note : String }


type alias GoalView =
    { goal : Goal, owner : Maybe String, active : Bool, evaluation : Evaluation, analysis : Analysis, results : List Measurement, strategies : List ( String, String ) }


type alias Authority =
    { owner : String, budgetLimit : Float, canHire : Bool, canChangePrice : Bool, canApprove : List String }


type alias Decision =
    { text : String, owner : String, deadline : Maybe String }


type alias Review =
    { id : String, goal : String, heldAt : String, note : String, evaluation : Evaluation, learnings : List String, decisions : List Decision }


type alias Diagnostic =
    { severity : String, code : String, message : String, subject : String, details : List String }


type alias Compiler =
    { errors : Int, warnings : Int, diagnostics : List Diagnostic }


type alias Node =
    { tag : String, contents : String }


type alias Edge =
    { from : Node, to : Node, kind : String }


type alias Audit =
    { seq : Int, at : String, actor : Maybe String, description : String, evaluatedGoal : Maybe String, evaluatedStatus : Maybe Status }


type alias ReviewWarning =
    { id : String, warnings : List String }


type alias Workspace =
    { organization : Organization, version : Int, demo : Bool, people : List Person, goals : List GoalView, authorities : List Authority, reviews : List Review, compiler : Compiler, edges : List Edge, events : List Audit, decisionShare : Dict String Float, reviewWarnings : List ReviewWarning }


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


statusName : Status -> String
statusName s =
    case s of
        NoData ->
            "결과 대기"

        OnTrack ->
            "정상"

        AtRisk ->
            "위험"

        OffTrack ->
            "이탈"

        Achieved ->
            "달성"


organizationDecoder : Decoder Organization
organizationDecoder =
    D.map3 Organization (D.field "id" D.string) (D.field "name" D.string) (D.field "createdAt" D.string)


summaryDecoder : Decoder Summary
summaryDecoder =
    D.succeed Summary |> field "organization" organizationDecoder |> field "demo" D.bool |> field "peopleCount" D.int |> field "goalCount" D.int


personDecoder : Decoder Person
personDecoder =
    D.succeed Person |> field "id" D.string |> field "name" D.string |> field "role" D.string


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
    D.succeed Measurement |> field "value" D.float |> field "reportedAt" D.string |> field "note" D.string


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


permissions : List ( String, String )
permissions =
    [ ( "Pricing", "가격 결정" ), ( "Hiring", "채용" ), ( "BudgetApproval", "예산 승인" ), ( "Contracting", "계약" ), ( "Marketing", "마케팅" ), ( "Infrastructure", "인프라" ), ( "ProductLaunch", "제품 출시" ) ]


permissionName : String -> String
permissionName key =
    List.filter (Tuple.first >> (==) key) permissions |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault key


personName : Workspace -> String -> String
personName w key =
    w.people |> List.filter (.id >> (==) key) |> List.head |> Maybe.map .name |> Maybe.withDefault key


goalName : Workspace -> String -> String
goalName w key =
    w.goals |> List.filter (.goal >> .id >> (==) key) |> List.head |> Maybe.map (.goal >> .description) |> Maybe.withDefault key
