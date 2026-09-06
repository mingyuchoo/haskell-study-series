module Page.Responsibility exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias Controls msg =
    { forms : Config msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, edges : List Edge, compiler : Compiler } -> Html msg
view model w =
    div []
        [ panel "누가 어떤 결과를 책임지는가" [ note "각 목표에는 최종 책임자가 한 명 있습니다.", div [ class "table-wrap" ] [ table [] [ thead [] [ tr [] (List.map (\title -> th [] [ text title ]) [ "결과 / KPI", "최종 책임자", "목표값", "필요 권한 / 통제율", "상태" ]) ], tbody [] (List.map (\g -> tr [ id ("owner-" ++ g.goal.id), tabindex -1 ] [ td [] [ strong [] [ text g.goal.description ], text g.goal.metric.name ], td [] [ formView model.forms (Assign g.goal.id) "책임자 지정" [ selectField model.forms (Assign g.goal.id) "책임자" "owner" True (peopleOptions w) ] ], td [] [ text (formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ], td [] [ text (String.join " · " (List.map permissionName g.goal.requiredPermissions)), note ("예산 " ++ formatNumber g.goal.requiredBudget ++ "원 · " ++ String.fromInt (round (g.analysis.coverage * 100)) ++ "% 통제") ], td [] [ badge g ] ]) w.goals) ] ] ]
        , section [ class "panel", id "responsibility-graph", tabindex -1 ]
            [ h2 [] [ text "책임 관계 그래프" ]
            , note "사람 → 목표 → 지표. 목표 간 의존 관계와 자원 통제를 연결합니다."
            , if List.isEmpty w.edges then
                note "책임자와 목표를 연결하면 그래프가 만들어집니다."

              else
                div [] (List.map (\edge -> div [ class "graph-edge" ] [ span [] [ text (nodeName w edge.from) ], b [] [ text ("─ " ++ edge.kind ++ " →") ], span [] [ text (nodeName w edge.to) ] ]) w.edges)
            ]
        , diagnosticView w
        ]


nodeName : { a | people : List Person, goals : List GoalView } -> Node -> String
nodeName w node =
    case node.tag of
        "PersonNode" ->
            personName w node.contents

        "GoalNode" ->
            goalName w node.contents

        _ ->
            node.contents
