module Ui.Label exposing (..)

import Domain exposing (..)


permissions : List ( String, String )
permissions =
    [ ( "Pricing", "가격 결정" ), ( "Hiring", "채용" ), ( "BudgetApproval", "예산 승인" ), ( "Contracting", "계약" ), ( "Marketing", "마케팅" ), ( "Infrastructure", "인프라" ), ( "ProductLaunch", "제품 출시" ) ]


permissionName : String -> String
permissionName key =
    List.filter (Tuple.first >> (==) key) permissions |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault key


personName : { a | people : List Person } -> String -> String
personName w key =
    w.people
        |> List.filter (.id >> (==) key)
        |> List.head
        |> Maybe.map
            (\p ->
                p.name
                    ++ (if p.active then
                            ""

                        else
                            " (비활성)"
                       )
            )
        |> Maybe.withDefault key


goalName : { a | goals : List GoalView } -> String -> String
goalName w key =
    w.goals |> List.filter (.goal >> .id >> (==) key) |> List.head |> Maybe.map (.goal >> .description) |> Maybe.withDefault key


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


formatNumber : Float -> String
formatNumber number =
    let
        parts =
            String.fromFloat (toFloat (round (abs number * 100)) / 100) |> String.split "."

        group reversed =
            if String.length reversed <= 3 then
                reversed

            else
                String.left 3 reversed ++ "," ++ group (String.dropLeft 3 reversed)

        whole =
            List.head parts |> Maybe.withDefault "0" |> String.reverse |> group |> String.reverse

        fraction =
            case List.drop 1 parts |> List.head of
                Just digits ->
                    "." ++ digits

                Nothing ->
                    ""
    in
    (if number < 0 then
        "-"

     else
        ""
    )
        ++ whole
        ++ fraction
