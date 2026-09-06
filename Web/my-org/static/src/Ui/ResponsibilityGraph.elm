module Ui.ResponsibilityGraph exposing (Msg(..), State, init, nodeKey, nodeLabel, nodes, relationLabel, update, view, visibleEdges)

import Dict
import Domain exposing (..)
import Html exposing (Html, button, div, h3, input, label, p, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, preventDefaultOn)
import Json.Decode as D
import Ui.Label exposing (..)
import VirtualDom


type alias State =
    { diagram : Bool, query : String, showDependencies : Bool, showResources : Bool, selected : Maybe String, zoom : Float }


type Msg
    = SetDiagram Bool
    | Search String
    | Dependencies Bool
    | Resources Bool
    | Select String
    | ClearSelection
    | Zoom Float
    | Fit


init : State
init =
    { diagram = True, query = "", showDependencies = False, showResources = False, selected = Nothing, zoom = 1 }


update : Msg -> State -> State
update msg state =
    case msg of
        SetDiagram value ->
            { state | diagram = value }

        Search value ->
            { state | query = value, selected = Nothing }

        Dependencies value ->
            { state | showDependencies = value }

        Resources value ->
            { state
                | showResources = value
                , selected =
                    if not value && (state.selected |> Maybe.map (String.startsWith "ResourceNode:") |> Maybe.withDefault False) then
                        Nothing

                    else
                        state.selected
            }

        Select key ->
            { state | selected = Just key }

        ClearSelection ->
            { state | selected = Nothing }

        Zoom delta ->
            { state | zoom = clamp 1 3 (state.zoom + delta) }

        Fit ->
            { state | zoom = 1 }


nodeKey : Node -> String
nodeKey node =
    node.tag ++ ":" ++ node.contents


nodes : { a | people : List Person, goals : List GoalView, edges : List Edge } -> List Node
nodes w =
    List.map (\person -> Node "PersonNode" person.id) w.people
        ++ List.map (\g -> Node "GoalNode" g.goal.id) w.goals
        ++ List.concatMap (\edge -> [ edge.from, edge.to ]) w.edges
        |> List.map (\node -> ( nodeKey node, node ))
        |> Dict.fromList
        |> Dict.values


visibleEdges : State -> List Edge -> List Edge
visibleEdges state =
    List.filter (\edge -> (edge.kind /= "DependsOn" || state.showDependencies) && (edge.kind /= "Controls" || state.showResources))


nodeLabel : { a | people : List Person, goals : List GoalView } -> Node -> String
nodeLabel w node =
    case node.tag of
        "PersonNode" ->
            personName w node.contents

        "GoalNode" ->
            goalName w node.contents

        "MetricNode" ->
            w.goals |> List.filter (.goal >> .metric >> .id >> (==) node.contents) |> List.head |> Maybe.map (.goal >> .metric >> .name) |> Maybe.withDefault node.contents

        "ResourceNode" ->
            if node.contents == "Budget" then
                "예산"

            else
                permissionName node.contents

        _ ->
            node.contents


relationLabel : String -> String
relationLabel kind =
    case kind of
        "Owns" ->
            "책임"

        "DependsOn" ->
            "하위 목표"

        "Measures" ->
            "측정 지표"

        "Controls" ->
            "보유 권한"

        _ ->
            kind


type alias Positioned =
    { node : Node, x : Float, y : Float }


layout : State -> { a | people : List Person, goals : List GoalView, edges : List Edge } -> List Positioned
layout state w =
    [ "PersonNode", "GoalNode", "MetricNode", "ResourceNode" ]
        |> List.indexedMap
            (\column tag ->
                nodes w
                    |> List.filter (\node -> node.tag == tag && (tag /= "ResourceNode" || state.showResources))
                    |> List.sortBy (nodeLabel w)
                    |> List.indexedMap (\row node -> { node = node, x = 24 + toFloat column * 340, y = 56 + toFloat row * 118 })
            )
        |> List.concat


matches : String -> { a | people : List Person, goals : List GoalView } -> Node -> Bool
matches query w node =
    String.contains (String.toLower (String.trim query)) (String.toLower (nodeLabel w node ++ " " ++ node.contents))


related : List Edge -> String -> String -> Bool
related edges selected key =
    selected == key || List.any (\edge -> (nodeKey edge.from == selected && nodeKey edge.to == key) || (nodeKey edge.to == selected && nodeKey edge.from == key)) edges


view : State -> Maybe (Msg -> msg) -> Maybe (String -> msg) -> { a | people : List Person, goals : List GoalView, edges : List Edge, authorities : List Authority } -> Html msg
view state dispatch go w =
    let
        positions =
            layout state w

        edges =
            visibleEdges state w.edges

        count =
            positions |> List.filter (.node >> matches state.query w) |> List.length
    in
    div [ class "responsibility-network" ]
        [ toolbar state dispatch
        , p [ class "note graph-legend" ] [ text "사람 → 책임 → 목표 → 측정 지표 | 점선 곡선: 상위 → 하위 목표 · 긴 점선: 사람 → 보유 권한/예산 | ⚠ 책임자 미지정·권한 부족" ]
        , p [ class "note", attribute "role" "status" ]
            [ text
                ("노드 "
                    ++ String.fromInt (List.length positions)
                    ++ "개 · 관계 "
                    ++ String.fromInt (List.length edges)
                    ++ "개"
                    ++ (if String.trim state.query == "" then
                            ""

                        else
                            " · 검색 일치 " ++ String.fromInt count ++ "개"
                       )
                )
            ]
        , if count == 0 && String.trim state.query /= "" then
            p [ class "note" ] [ text "검색 조건에 맞는 노드가 없습니다. 검색어를 변경하세요." ]

          else
            text ""
        , div [ classList [ ( "graph-content", True ), ( "has-selection", state.selected /= Nothing ) ] ]
            [ if List.isEmpty positions then
                p [] [ text "아직 구성원과 목표가 없습니다. 구성원이나 목표를 추가하면 관계를 확인할 수 있습니다." ]

              else if state.diagram then
                diagram state dispatch w positions edges

              else
                relationList state dispatch w positions edges
            , selectionDetails state dispatch go w
            ]
        ]


toolbar state dispatch =
    let
        action message =
            dispatch |> Maybe.map (\send -> [ onClick (send message) ]) |> Maybe.withDefault []

        toggle title active message =
            button ([ type_ "button", classList [ ( "secondary", not active ) ], attribute "aria-pressed" (bool active), disabled (dispatch == Nothing) ] ++ action message) [ text title ]

        check title value constructor =
            label [] [ input ([ type_ "checkbox", checked value, disabled (dispatch == Nothing) ] ++ (dispatch |> Maybe.map (\send -> [ onCheck (constructor >> send) ]) |> Maybe.withDefault [])) [], text title ]
    in
    div [ class "graph-toolbar" ]
        [ div [ class "graph-view-toggle", attribute "role" "group", attribute "aria-label" "책임 관계 보기" ] [ toggle "다이어그램" state.diagram (SetDiagram True), toggle "관계 목록" (not state.diagram) (SetDiagram False) ]
        , label [ class "graph-search" ] [ text "관계 검색", input ([ type_ "search", value state.query, placeholder "사람, 목표, 지표, 권한 검색", disabled (dispatch == Nothing) ] ++ (dispatch |> Maybe.map (\send -> [ onInput (Search >> send) ]) |> Maybe.withDefault [])) [] ]
        , div [ class "graph-options" ] [ check "목표 간 관계" state.showDependencies Dependencies, check "권한·예산" state.showResources Resources ]
        , div [ class "graph-zoom", attribute "role" "group", attribute "aria-label" "다이어그램 확대" ]
            [ button ([ type_ "button", class "secondary", disabled (state.zoom <= 1 || dispatch == Nothing), attribute "aria-label" "축소" ] ++ action (Zoom -0.25)) [ text "−" ]
            , span [] [ text (String.fromInt (round (state.zoom * 100)) ++ "%") ]
            , button ([ type_ "button", class "secondary", disabled (state.zoom >= 3 || dispatch == Nothing), attribute "aria-label" "확대" ] ++ action (Zoom 0.25)) [ text "+" ]
            , button ([ type_ "button", class "secondary", disabled (dispatch == Nothing) ] ++ action Fit) [ text "전체 맞춤" ]
            ]
        ]


bool value =
    if value then
        "true"

    else
        "false"


svg =
    VirtualDom.nodeNS "http://www.w3.org/2000/svg"


diagram state dispatch w positions edges =
    let
        width =
            if state.showResources then
                1360

            else
                1020

        height =
            positions |> List.map (\pos -> pos.y + 110) |> List.maximum |> Maybe.withDefault 180
    in
    div []
        [ p [ class "note" ] [ text "노드를 선택하면 연결 관계를 강조합니다. Tab과 Enter/Space로도 선택할 수 있습니다. 확대 후 스크롤로 이동하세요." ]
        , div [ class "graph-viewport", tabindex 0, attribute "role" "region", attribute "aria-label" "책임 관계 다이어그램 · 스크롤 탐색" ]
            [ svg "svg"
                [ attribute "viewBox" ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
                , attribute "width" (String.fromFloat (state.zoom * 100) ++ "%")
                , attribute "class"
                    (if state.zoom == 1 then
                        "graph-svg graph-fit"

                     else
                        "graph-svg"
                    )
                , attribute "preserveAspectRatio" "xMidYMin meet"
                , attribute "role" "group"
                , attribute "aria-label" "사람, 목표, 지표와 자원 관계"
                ]
                ([ svg "defs" [] [ svg "marker" [ attribute "id" "responsibility-arrow", attribute "viewBox" "0 0 10 10", attribute "refX" "9", attribute "refY" "5", attribute "markerWidth" "7", attribute "markerHeight" "7", attribute "orient" "auto-start-reverse" ] [ svg "path" [ attribute "d" "M 0 0 L 10 5 L 0 10 z", attribute "fill" "#557467" ] [] ] ] ]
                    ++ List.indexedMap (\i name -> svg "text" [ attribute "x" (String.fromInt (24 + i * 340)), attribute "y" "29", attribute "class" "graph-column" ] [ text name ])
                        (if state.showResources then
                            [ "사람", "목표", "지표", "권한 · 예산" ]

                         else
                            [ "사람", "목표", "지표" ]
                        )
                    ++ List.filterMap (edgeView state positions) edges
                    ++ List.map (nodeView state dispatch w edges) positions
                )
            ]
        ]


edgeView state positions edge =
    let
        find node =
            positions |> List.filter (.node >> nodeKey >> (==) (nodeKey node)) |> List.head

        active =
            state.selected |> Maybe.map (\key -> nodeKey edge.from == key || nodeKey edge.to == key) |> Maybe.withDefault True
    in
    Maybe.map2
        (\from to ->
            let
                x1 =
                    from.x + 260

                y1 =
                    from.y + 42

                x2 =
                    to.x

                y2 =
                    to.y + 42

                d =
                    if edge.kind == "DependsOn" then
                        "M " ++ pair x1 y1 ++ " C " ++ pair (x1 + 55) y1 ++ " " ++ pair (x1 + 55) y2 ++ " " ++ pair (x1 + 3) y2

                    else if edge.kind == "Controls" then
                        "M " ++ pair x1 (y1 + 25) ++ " C " ++ pair (x1 + 45) (y1 + 65) ++ " " ++ pair (x2 - 45) (y2 + 65) ++ " " ++ pair x2 (y2 + 25)

                    else
                        "M " ++ pair x1 y1 ++ " C " ++ pair (x1 + 40) y1 ++ " " ++ pair (x2 - 40) y2 ++ " " ++ pair x2 y2
            in
            svg "g"
                [ attribute "opacity"
                    (if active then
                        "1"

                     else
                        "0.16"
                    )
                ]
                [ svg "title" [] [ text (relationLabel edge.kind) ]
                , svg "path"
                    [ attribute "d" d
                    , attribute "fill" "none"
                    , attribute "stroke" "#557467"
                    , attribute "stroke-width"
                        (if state.selected /= Nothing && active then
                            "3"

                         else
                            "1.5"
                        )
                    , attribute "stroke-dasharray"
                        (if edge.kind == "DependsOn" || edge.kind == "Controls" then
                            "6 4"

                         else
                            "none"
                        )
                    , attribute "marker-end" "url(#responsibility-arrow)"
                    ]
                    []
                ]
        )
        (find edge.from)
        (find edge.to)


pair x y =
    String.fromFloat x ++ " " ++ String.fromFloat y


nodeView state dispatch w edges pos =
    let
        key =
            nodeKey pos.node

        selected =
            state.selected == Just key

        illuminated =
            Maybe.map (\chosen -> related edges chosen key) state.selected |> Maybe.withDefault True

        found =
            matches state.query w pos.node

        warning =
            goalWarning w pos.node

        title =
            nodeLabel w pos.node

        events =
            dispatch
                |> Maybe.map
                    (\send ->
                        [ onClick (send (Select key))
                        , preventDefaultOn "keydown"
                            (D.field "key" D.string
                                |> D.andThen
                                    (\pressed ->
                                        if pressed == "Enter" || pressed == " " then
                                            D.succeed ( send (Select key), True )

                                        else
                                            D.fail "not an activation key"
                                    )
                            )
                        ]
                    )
                |> Maybe.withDefault []

        color =
            if warning /= "" then
                "#fff2dc"

            else if pos.node.tag == "PersonNode" then
                "#eaf3ec"

            else if pos.node.tag == "MetricNode" then
                "#eaf1fa"

            else if pos.node.tag == "ResourceNode" then
                "#f1edf8"

            else
                "#fff"
    in
    svg "g"
        ([ attribute "transform" ("translate(" ++ pair pos.x pos.y ++ ")")
         , attribute "class" "graph-node"
         , attribute "role" "button"
         , attribute "tabindex" "0"
         , attribute "aria-label"
            (title
                ++ (if warning == "" then
                        ""

                    else
                        " · " ++ warning
                   )
            )
         , attribute "aria-pressed" (bool selected)
         , attribute "opacity"
            (if illuminated && found then
                "1"

             else
                "0.3"
            )
         ]
            ++ events
        )
        [ svg "title" [] [ text (title ++ " · " ++ warning) ]
        , svg "rect"
            [ attribute "width" "260"
            , attribute "height" "84"
            , attribute "rx" "10"
            , attribute "fill" color
            , attribute "stroke"
                (if selected then
                    "#1c6147"

                 else if found && String.trim state.query /= "" then
                    "#3479b3"

                 else
                    "#b8cbbd"
                )
            , attribute "stroke-width"
                (if selected || String.trim state.query /= "" && found then
                    "3"

                 else
                    "1.5"
                )
            ]
            []
        , svg "text"
            [ attribute "x" "14", attribute "y" "26", attribute "class" "graph-node-label" ]
            (wrapped title
                |> List.indexedMap
                    (\i line ->
                        svg "tspan"
                            [ attribute "x" "14"
                            , attribute "dy"
                                (if i == 0 then
                                    "0"

                                 else
                                    "19"
                                )
                            ]
                            [ text line ]
                    )
            )
        , svg "text"
            [ attribute "x" "14", attribute "y" "71", attribute "class" "graph-node-meta" ]
            [ text
                (if warning /= "" then
                    "⚠ " ++ warning

                 else
                    nodeType pos.node
                )
            ]
        ]


wrapped value =
    if String.length value <= 17 then
        [ value ]

    else
        [ String.left 17 value
        , String.slice 17 33 value
            ++ (if String.length value > 33 then
                    "…"

                else
                    ""
               )
        ]


nodeType node =
    case node.tag of
        "PersonNode" ->
            "사람"

        "GoalNode" ->
            "목표"

        "MetricNode" ->
            "측정 지표"

        _ ->
            "권한 · 예산"


goalWarning w node =
    if node.tag /= "GoalNode" then
        ""

    else
        w.goals
            |> List.filter (.goal >> .id >> (==) node.contents)
            |> List.head
            |> Maybe.map
                (\g ->
                    if g.owner == Nothing then
                        "책임자 미지정"

                    else if g.analysis.coverage < 1 then
                        "권한 부족"

                    else
                        ""
                )
            |> Maybe.withDefault ""


relationList state dispatch w positions edges =
    let
        shown =
            edges |> List.filter (\edge -> matches state.query w edge.from || matches state.query w edge.to)

        pick node =
            button ([ type_ "button", class "secondary" ] ++ (dispatch |> Maybe.map (\send -> [ onClick (send (Select (nodeKey node))) ]) |> Maybe.withDefault [])) [ text (nodeLabel w node) ]

        isolated =
            positions |> List.filter (\pos -> not (List.any (\edge -> nodeKey edge.from == nodeKey pos.node || nodeKey edge.to == nodeKey pos.node) edges) && matches state.query w pos.node)
    in
    div [ class "graph-relations" ]
        (List.map (\edge -> div [ class "graph-edge" ] [ pick edge.from, span [] [ text ("─ " ++ relationLabel edge.kind ++ " →") ], pick edge.to ]) shown
            ++ List.map (\pos -> div [ class "graph-edge" ] [ pick pos.node, span [] [ text "표시 중인 연결 없음" ] ]) isolated
            ++ (if List.isEmpty shown && List.isEmpty isolated then
                    [ p [] [ text "검색 조건에 맞는 관계가 없습니다." ] ]

                else
                    []
               )
        )


selectionDetails state dispatch go w =
    case state.selected |> Maybe.andThen (\key -> nodes w |> List.filter (nodeKey >> (==) key) |> List.head) of
        Nothing ->
            p [ class "note" ] [ text "노드를 선택하면 전체 이름, 상태와 연결된 관계를 확인할 수 있습니다." ]

        Just node ->
            let
                edges =
                    List.filter (\edge -> edge.from == node || edge.to == node) w.edges

                goal =
                    w.goals |> List.filter (.goal >> .id >> (==) node.contents) |> List.head

                details =
                    if node.tag == "GoalNode" then
                        goal
                            |> Maybe.map
                                (\g ->
                                    [ p [] [ text ("책임자: " ++ (g.owner |> Maybe.map (personName w) |> Maybe.withDefault "미지정")) ]
                                    , p []
                                        [ text
                                            ((if g.active then
                                                "활성"

                                              else
                                                "초안"
                                             )
                                                ++ " · "
                                                ++ statusName g.evaluation.status
                                                ++ " · 권한 통제율 "
                                                ++ String.fromInt (round (g.analysis.coverage * 100))
                                                ++ "%"
                                            )
                                        ]
                                    , p [] [ text ("필요 권한: " ++ String.join ", " (List.map permissionName g.goal.requiredPermissions) ++ " · 필요 예산 " ++ formatNumber g.goal.requiredBudget ++ "원") ]
                                    , button ([ type_ "button", disabled (go == Nothing) ] ++ (go |> Maybe.map (\navigate -> [ onClick (navigate ("owner-" ++ node.contents)) ]) |> Maybe.withDefault [])) [ text "책임자 지정 폼으로 이동" ]
                                    ]
                                )
                            |> Maybe.withDefault []

                    else if node.tag == "PersonNode" then
                        personDetails go w node

                    else
                        []
            in
            div [ class "graph-detail", attribute "aria-live" "polite" ]
                ([ h3 [] [ text (nodeLabel w node) ], p [ class "note" ] [ text (nodeType node ++ " · 전체 연결 " ++ String.fromInt (List.length edges) ++ "개 (숨긴 관계 포함)") ] ]
                    ++ details
                    ++ List.map (\edge -> p [] [ text (nodeLabel w edge.from ++ " ─ " ++ relationLabel edge.kind ++ " → " ++ nodeLabel w edge.to) ]) edges
                    ++ (dispatch |> Maybe.map (\send -> [ button [ type_ "button", class "secondary", onClick (send ClearSelection) ] [ text "선택 해제" ] ]) |> Maybe.withDefault [])
                )


personDetails go w node =
    let
        person =
            w.people |> List.filter (.id >> (==) node.contents) |> List.head

        navigate target title =
            button ([ type_ "button", disabled (go == Nothing) ] ++ (go |> Maybe.map (\send -> [ onClick (send target) ]) |> Maybe.withDefault [])) [ text title ]

        owned =
            w.goals |> List.filter (.owner >> (==) (Just node.contents))
    in
    (person
        |> Maybe.map
            (\p ->
                [ Html.p []
                    [ text
                        ("역할: "
                            ++ p.role
                            ++ " · "
                            ++ (if p.active then
                                    "재직"

                                else
                                    "비활성"
                               )
                        )
                    ]
                ]
            )
        |> Maybe.withDefault []
    )
        ++ [ p [] [ text ("담당 목표 " ++ String.fromInt (List.length owned) ++ "개") ] ]
        ++ (w.authorities |> List.filter (.owner >> (==) node.contents) |> List.head |> Maybe.map (\authority -> [ p [] [ text ("보유 예산 " ++ formatNumber authority.budgetLimit ++ "원") ] ]) |> Maybe.withDefault [ p [] [ text "부여된 권한 정보 없음" ] ])
        ++ [ navigate ("person:" ++ node.contents) "구성원 상세로 이동" ]
        ++ (if person |> Maybe.map .active |> Maybe.withDefault False then
                [ navigate ("authority-" ++ node.contents) "권한 관리로 이동" ]

            else
                []
           )
