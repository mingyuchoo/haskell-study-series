module Ui.AgentGraph exposing (layers, view)

import Dict exposing (Dict)
import Domain exposing (Diagnostic)
import Domain.Agent as Agent exposing (Role, levelLabel)
import Html exposing (Html, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, tabindex)
import Set exposing (Set)
import Ui.Label exposing (permissionName, personName)
import VirtualDom


{-| 인계 관계로 층을 나눈다. 인계를 받지 않는 역할이 첫 층이고, 인계를 따라
내려간다. 순환이나 고립된 역할은 마지막 층에 둔다.
-}
layers : List Role -> List (List Role)
layers roles =
    let
        ids =
            Set.fromList (List.map .id roles)

        incoming =
            List.foldl (\role acc -> List.foldl (\target -> Dict.update target (Maybe.withDefault 0 >> (+) 1 >> Just)) acc (List.filter (\t -> Set.member t ids) role.handoffTo)) Dict.empty roles

        byId =
            Dict.fromList (List.map (\role -> ( role.id, role )) roles)

        step remaining counts acc =
            if List.isEmpty remaining then
                List.reverse acc

            else
                let
                    ready =
                        List.filter (\role -> Dict.get role.id counts == Nothing || Dict.get role.id counts == Just 0) remaining
                in
                if List.isEmpty ready then
                    List.reverse (remaining :: acc)

                else
                    let
                        readyIds =
                            Set.fromList (List.map .id ready)

                        next =
                            List.filter (\role -> not (Set.member role.id readyIds)) remaining

                        released =
                            List.foldl (\role c -> List.foldl (\target -> Dict.update target (Maybe.map (\n -> n - 1))) c role.handoffTo) counts ready
                    in
                    step next released (ready :: acc)
    in
    step (List.filterMap (\role -> Dict.get role.id byId) roles) incoming []


type alias Positioned =
    { role : Role, x : Float, y : Float }


svg =
    VirtualDom.nodeNS "http://www.w3.org/2000/svg"


approvalLabel : { a | people : List Domain.Person } -> Agent.Approval -> String
approvalLabel w approval =
    case approval of
        Agent.Person uid ->
            personName w uid ++ " 승인"

        Agent.Permission permission ->
            permissionName permission ++ " 권한자 승인"


view : { a | people : List Domain.Person } -> List Role -> List Diagnostic -> Html msg
view w roles diagnostics =
    let
        columns =
            layers roles

        positions =
            columns
                |> List.indexedMap (\column layer -> List.indexedMap (\row role -> { role = role, x = 24 + toFloat column * 300, y = 40 + toFloat row * 132 }) layer)
                |> List.concat

        lookup =
            Dict.fromList (List.map (\pos -> ( pos.role.id, pos )) positions)

        width =
            toFloat (max 1 (List.length columns)) * 300 + 40

        height =
            positions |> List.map (\pos -> pos.y + 130) |> List.maximum |> Maybe.withDefault 160

        issues role =
            List.filter (\d -> d.subject == role.id) diagnostics

        node pos =
            let
                role =
                    pos.role

                flagged =
                    List.any (\d -> d.severity == "Error") (issues role)
            in
            svg "g"
                [ attribute "class" "graph-node agent-node"
                , attribute "transform" ("translate(" ++ String.fromFloat pos.x ++ "," ++ String.fromFloat pos.y ++ ")")
                , attribute "role" "listitem"
                , tabindex 0
                , attribute "aria-label" (role.name ++ " · " ++ levelLabel role.level ++ (role.approval |> Maybe.map (\a -> " · " ++ approvalLabel w a) |> Maybe.withDefault ""))
                ]
                [ svg "rect"
                    [ attribute "width" "240"
                    , attribute "height" "100"
                    , attribute "rx" "10"
                    , attribute "fill" "#fff"
                    , attribute "stroke"
                        (if flagged then
                            "#bd7769"

                         else
                            "#8fb47e"
                        )
                    , attribute "stroke-width" "2"
                    ]
                    []
                , svg "text" [ attribute "x" "14", attribute "y" "28", attribute "class" "graph-node-label" ] [ text (String.left 18 role.name) ]
                , svg "text" [ attribute "x" "14", attribute "y" "50", attribute "class" "graph-node-meta" ] [ text (levelLabel role.level) ]
                , svg "text" [ attribute "x" "14", attribute "y" "70", attribute "class" "graph-node-meta" ] [ text (role.approval |> Maybe.map (approvalLabel w) |> Maybe.withDefault "사람 승인 없음 또는 미확인") ]
                , svg "text"
                    [ attribute "x" "14", attribute "y" "90", attribute "class" "graph-node-meta" ]
                    [ text
                        (if List.isEmpty (issues role) then
                            "진단 없음"

                         else
                            String.fromInt (List.length (issues role)) ++ "건 확인 필요"
                        )
                    ]
                ]

        edge pos target =
            case Dict.get target lookup of
                Just to ->
                    svg "line"
                        [ attribute "x1" (String.fromFloat (pos.x + 240))
                        , attribute "y1" (String.fromFloat (pos.y + 50))
                        , attribute "x2" (String.fromFloat to.x)
                        , attribute "y2" (String.fromFloat (to.y + 50))
                        , attribute "stroke" "#466253"
                        , attribute "stroke-width" "2"
                        , attribute "marker-end" "url(#agent-arrow)"
                        ]
                        []

                Nothing ->
                    text ""

        edges =
            List.concatMap (\pos -> List.map (edge pos) pos.role.handoffTo) positions
    in
    div [ class "agent-graph" ]
        [ p [ class "note graph-legend" ] [ text "왼쪽에서 오른쪽으로 산출물이 인계됩니다. 붉은 테두리는 오류 진단이 있는 역할입니다. 사람 승인은 각 역할 안에 표시합니다." ]
        , if List.isEmpty roles then
            p [] [ text "아직 표시할 에이전트 역할이 없습니다. 업무 흐름을 저장하면 규칙 기반 초안이 나타나고, 에이전트 초안 화면에서 설계안을 저장할 수 있습니다." ]

          else
            div [ class "graph-viewport", tabindex 0, attribute "role" "region", attribute "aria-label" "에이전트 인계 구조 다이어그램" ]
                [ svg "svg"
                    [ attribute "viewBox" ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
                    , attribute "width" "100%"
                    , attribute "class" "graph-svg graph-fit"
                    , attribute "role" "list"
                    ]
                    (svg "defs" [] [ svg "marker" [ attribute "id" "agent-arrow", attribute "markerWidth" "10", attribute "markerHeight" "10", attribute "refX" "9", attribute "refY" "5", attribute "orient" "auto" ] [ svg "path" [ attribute "d" "M0,0 L10,5 L0,10 z", attribute "fill" "#466253" ] [] ] ]
                        :: edges
                        ++ List.map node positions
                    )
                ]
        , ul [ class "graph-relations" ]
            (List.concatMap
                (\role ->
                    List.map
                        (\target ->
                            li [ class "graph-edge" ] [ strong [] [ text role.name ], span [] [ text "→ 인계 →" ], strong [] [ text (Dict.get target lookup |> Maybe.map (.role >> .name) |> Maybe.withDefault target) ] ]
                        )
                        role.handoffTo
                )
                roles
            )
        ]
