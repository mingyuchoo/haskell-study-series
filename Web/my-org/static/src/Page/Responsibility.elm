module Page.Responsibility exposing (view, viewInteractive, viewWith)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)
import Ui.ListView as ListView exposing (Mode(..))
import Ui.ResponsibilityGraph as Graph


type alias Controls msg =
    { forms : Config msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, edges : List Edge, compiler : Compiler, authorities : List Authority } -> Html msg
view =
    viewWith Table


viewWith : Mode -> Controls msg -> { a | goals : List GoalView, people : List Person, edges : List Edge, compiler : Compiler, authorities : List Authority } -> Html msg
viewWith mode model w =
    viewInteractive mode Graph.init Nothing Nothing model w


viewInteractive mode graphState graphMsg go model w =
    div []
        [ panel "누가 어떤 결과를 책임지는가"
            [ note "각 목표에는 최종 책임자가 한 명 있습니다."
            , if List.isEmpty w.goals then
                emptyState "아직 책임을 배정할 목표가 없습니다" "목표 메뉴에서 목표를 만든 뒤 책임자를 지정하세요."

              else if mode == Table then
                ListView.tableView "목표별 책임" [ "결과 / KPI", "최종 책임자", "목표값", "필요 권한 / 통제율", "상태" ] (List.map (responsibilityRow model w) w.goals)

              else
                div [ class "grid" ] (List.map (responsibilityCard model w) w.goals)
            ]
        , section [ class "panel", id "responsibility-graph", tabindex -1 ]
            [ h2 [] [ text "책임 관계 그래프" ]
            , note "사람 → 목표 → 지표. 목표 간 의존 관계와 자원 통제를 연결합니다."
            , Graph.view graphState graphMsg go w
            ]
        , diagnosticView w
        ]


responsibilityRow model w g =
    tr [ id ("owner-" ++ g.goal.id), tabindex -1 ]
        [ th [ scope "row" ] [ strong [] [ text g.goal.description ], small [] [ text g.goal.metric.name ] ]
        , td [] [ note (g.owner |> Maybe.map (personName w) |> Maybe.withDefault "책임자 미지정"), ownerForm model w g ]
        , td [] [ text (formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ]
        , td [] (requirements g)
        , td [] [ badge g ]
        ]


responsibilityCard model w g =
    article [ class "goal-card", id ("owner-" ++ g.goal.id), tabindex -1 ]
        ([ badge g, h2 [] [ text g.goal.description ], small [] [ text g.goal.metric.name ], p [] [ text ("목표값 " ++ formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ], note ("최종 책임자: " ++ (g.owner |> Maybe.map (personName w) |> Maybe.withDefault "책임자 미지정")), ownerForm model w g ] ++ requirements g)


ownerForm model w g =
    formView model.forms (Assign g.goal.id) "책임자 지정" [ selectField model.forms (Assign g.goal.id) "책임자" "owner" True (peopleOptions w) ]


requirements g =
    [ p [] [ text (String.join " · " (List.map permissionName g.goal.requiredPermissions)) ], note ("예산 " ++ formatNumber g.goal.requiredBudget ++ "원 · " ++ String.fromInt (round (g.analysis.coverage * 100)) ++ "% 통제") ]
