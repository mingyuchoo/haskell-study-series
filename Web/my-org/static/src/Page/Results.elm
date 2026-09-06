module Page.Results exposing (view, viewWith)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)
import Ui.ListView as ListView exposing (Mode(..))


type alias Controls msg =
    { forms : Config msg, goals : String -> msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person } -> Html msg
view =
    viewWith Table


viewWith : Mode -> Controls msg -> { a | goals : List GoalView, people : List Person } -> Html msg
viewWith mode model w =
    div []
        [ div [ class "section-head" ] [ h2 [] [ text "목표별 결과와 평가" ] ]
        , note "실측값을 보고하고 현재 성과를 평가하세요. 결과 이력은 다음 학습의 근거가 됩니다."
        , if List.isEmpty w.goals then
            emptyState "아직 측정할 목표가 없습니다" "목표 메뉴에서 목표를 만든 뒤 결과를 기록하세요."

          else if mode == Table then
            goalTable model w

          else
            div [ class "grid" ] (List.map (resultCard model w) w.goals)
        ]


resultCard : Controls msg -> { a | goals : List GoalView, people : List Person } -> GoalView -> Html msg
resultCard model w g =
    article [ class "goal-card", id ("goal-" ++ g.goal.id), tabindex -1 ]
        (goalSummary w g ++ resultContent model w g)


resultContent model w g =
    [ note g.analysis.possibleCause
    , h3 [ class "form-heading" ] [ text "결과 보고" ]
    , formView model.forms (Report g.goal.id) "결과 보고" [ div [ class "fields" ] [ inputField model.forms (Report g.goal.id) "실측값" "value" "number" True, selectField model.forms (Report g.goal.id) "보고자" "reportedBy" True (peopleOptions w) ], inputField model.forms (Report g.goal.id) "결과 설명" "note" "text" True ]
    , if List.isEmpty g.results then
        note "아직 결과가 없습니다."

      else
        div [ class "table-wrap" ] [ h3 [ class "form-heading" ] [ text "결과 추이 · 최근 순" ], table [] [ thead [] [ tr [] [ th [] [ text "기록 시각" ], th [] [ text "측정값" ], th [] [ text "보고자" ], th [] [ text "설명" ] ] ], tbody [] (List.map (\r -> tr [] [ td [] [ text r.reportedAt ], td [] [ text (formatNumber r.value) ], td [] [ text (r.reportedBy |> Maybe.map (personName w) |> Maybe.withDefault "미기록") ], td [] [ text r.note ] ]) g.results) ] ]
    , div [ class "actions" ]
        [ button [ class "secondary", disabled (model.forms.busy || not model.forms.fresh), onClick (model.forms.submit (Evaluate g.goal.id)) ] [ text "평가 기록" ]
        , button [ class "secondary", disabled model.forms.busy, onClick (model.goals ("goal-" ++ g.goal.id)) ] [ text "목표 관리 →" ]
        ]
    ]


goalTable model w =
    ListView.tableView "목표별 결과와 평가"
        [ "목표 / KPI", "최종 책임자", "현재값 / 목표값", "달성률", "마감", "상태" ]
        (List.concatMap
            (\g ->
                [ tr [ id ("goal-" ++ g.goal.id), tabindex -1 ]
                    [ th [ scope "row" ]
                        [ strong [] [ text g.goal.description ]
                        , small []
                            [ text
                                (g.goal.metric.name
                                    ++ " · "
                                    ++ (if g.goal.metric.direction == "HigherIsBetter" then
                                            "↑ 증가"

                                        else
                                            "↓ 감소"
                                       )
                                )
                            ]
                        ]
                    , td [] [ text (g.owner |> Maybe.map (personName w) |> Maybe.withDefault "책임자 미지정") ]
                    , td [] [ text ((g.evaluation.latestValue |> Maybe.map formatNumber |> Maybe.withDefault "—") ++ " / " ++ formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ]
                    , td [] [ text (String.fromInt (round (g.evaluation.progress * 100)) ++ "%"), small [] [ text ("기준 " ++ formatNumber g.goal.baseline) ] ]
                    , td [] [ text (String.left 10 g.goal.deadline) ]
                    , td [] [ badge g ]
                    ]
                , ListView.detailRow 6 [] (g.goal.description ++ " · 결과 보고 · 평가 · 이력") (resultContent model w g)
                ]
            )
            w.goals
        )
