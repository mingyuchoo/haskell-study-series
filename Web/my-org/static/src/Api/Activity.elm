module Api.Activity exposing (decoder, empty, unscoped)

import Domain exposing (Activity)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set


empty : Activity
empty =
    { tag = "", targetKind = "", targetId = "", personId = Nothing, detail = "", reviewId = Nothing, raw = "null" }


decoder : Decoder Activity
decoder =
    D.value |> D.map (\raw -> interpret (unscoped raw) |> (\event -> { event | raw = E.encode 2 raw }))


unscoped : D.Value -> D.Value
unscoped raw =
    if read (D.field "tag" D.string) raw == "OrganizationScoped" then
        D.decodeValue (D.field "contents" (D.index 1 D.value)) raw |> Result.map unscoped |> Result.withDefault raw

    else
        raw


read : Decoder String -> D.Value -> String
read decoder_ raw =
    D.decodeValue decoder_ raw |> Result.withDefault ""


interpret : D.Value -> Activity
interpret raw =
    let
        tag =
            read (D.field "tag" D.string) raw

        contents =
            D.decodeValue (D.field "contents" D.value) raw |> Result.withDefault E.null

        at n decoder_ =
            D.index n decoder_

        str =
            read D.string contents

        first =
            read (at 0 D.string) contents

        field key =
            read (D.field key D.string) contents

        target kind ident =
            { empty | tag = tag, targetKind = kind, targetId = ident }

        pair kind =
            target kind first

        objectAt n key =
            read (at n (D.field key D.string)) contents

        detail decoder_ =
            read decoder_ contents

        number =
            D.float |> D.map String.fromFloat

        personEvent =
            let
                event =
                    target "person" (objectAt 0 "id")
            in
            { event | detail = objectAt 0 "name" ++ " · " ++ objectAt 0 "role" }
    in
    case tag of
        "OrganizationCreated" ->
            let
                event =
                    target "organization" (field "id")
            in
            { event | detail = field "name" }

        "OrganizationRenamed" ->
            let
                event =
                    pair "organization"
            in
            { event | detail = detail (at 1 D.string) }

        "OrganizationDeleted" ->
            target "organization" str

        "DemoSeeded" ->
            target "organization" str

        "PersonAdded" ->
            let
                event =
                    target "person" (field "id")
            in
            { event | detail = field "name" ++ " · " ++ field "role" }

        "EmployeeAdded" ->
            personEvent

        "PersonUpdated" ->
            personEvent

        "PersonDeactivated" ->
            let
                event =
                    pair "person"
            in
            { event | personId = D.decodeValue (at 1 (D.nullable D.string)) contents |> Result.withDefault Nothing }

        "GoalCreated" ->
            let
                event =
                    target "goal" (field "id")
            in
            { event | detail = field "description" }

        "OwnerAssigned" ->
            let
                event =
                    pair "goal"
            in
            { event | personId = Just (detail (at 1 D.string)) }

        "AuthorityGranted" ->
            let
                event =
                    pair "person"

                flags =
                    [ ( "canHire", "채용" ), ( "canChangePrice", "가격 결정" ) ]
                        |> List.filterMap
                            (\( key, label ) ->
                                if D.decodeValue (at 1 (D.field key D.bool)) contents == Ok True then
                                    Just label

                                else
                                    Nothing
                            )

                approvals =
                    D.decodeValue (at 1 (D.field "canApprove" (D.list D.string))) contents |> Result.withDefault [] |> List.map permissionLabel

                granted =
                    Set.fromList (flags ++ approvals) |> Set.toList
            in
            { event
                | detail =
                    "예산 "
                        ++ detail (at 1 (D.field "budgetLimit" number))
                        ++ " · 보유 권한 전체: "
                        ++ (if List.isEmpty granted then
                                "없음"

                            else
                                String.join ", " granted
                           )
            }

        "AuthorityRevoked" ->
            let
                event =
                    pair "person"
            in
            { event | detail = permissionLabel (detail (at 1 D.string)) }

        "GoalActivated" ->
            target "goal" str

        "ResultReported" ->
            let
                event =
                    pair "goal"
            in
            { event | detail = "값 " ++ detail (at 1 (D.field "value" number)) ++ " · " ++ objectAt 1 "note", personId = D.decodeValue (at 1 (D.field "reportedBy" (D.nullable D.string))) contents |> Result.withDefault Nothing }

        "GoalEvaluated" ->
            let
                event =
                    pair "goal"
            in
            { event | detail = statusLabel (objectAt 1 "status") ++ " · 진행률 " ++ detail (at 1 (D.field "progress" (D.float |> D.map (\n -> String.fromFloat (toFloat (round (n * 10000)) / 100) ++ "%")))) }

        "ReviewHeld" ->
            let
                event =
                    target "goal" (field "goal")
            in
            { event | reviewId = Just (field "id"), detail = field "note" ++ " · 결정 " ++ detail (D.field "decisions" (D.list D.value |> D.map (List.length >> String.fromInt))) ++ "건 · 학습 " ++ detail (D.field "learnings" (D.list D.value |> D.map (List.length >> String.fromInt))) ++ "건" }

        "StrategyChanged" ->
            let
                event =
                    pair "goal"
            in
            { event | detail = detail (at 1 D.string) }

        _ ->
            { empty | tag = tag }


permissionLabel : String -> String
permissionLabel key =
    [ ( "Pricing", "가격 결정" ), ( "Hiring", "채용" ), ( "BudgetApproval", "예산 승인" ), ( "Contracting", "계약" ), ( "Marketing", "마케팅" ), ( "Infrastructure", "인프라" ), ( "ProductLaunch", "제품 출시" ) ]
        |> List.filter (Tuple.first >> (==) key)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault key


statusLabel : String -> String
statusLabel key =
    [ ( "NoData", "결과 대기" ), ( "OnTrack", "정상" ), ( "AtRisk", "위험" ), ( "OffTrack", "이탈" ), ( "Achieved", "달성" ) ]
        |> List.filter (Tuple.first >> (==) key)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault key
