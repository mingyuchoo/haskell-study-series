module Page.Results exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias Controls msg =
    { forms : Config msg, goals : String -> msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person } -> Html msg
view model w =
    div []
        [ div [ class "section-head" ] [ h2 [] [ text "목표별 결과와 평가" ] ]
        , note "실측값을 보고하고 현재 성과를 평가하세요. 결과 이력은 다음 학습의 근거가 됩니다."
        , if List.isEmpty w.goals then
            emptyState "아직 측정할 목표가 없습니다" "목표 메뉴에서 목표를 만든 뒤 결과를 기록하세요."

          else
            div [ class "grid" ] (List.map (resultCard model w) w.goals)
        ]


resultCard : Controls msg -> { a | goals : List GoalView, people : List Person } -> GoalView -> Html msg
resultCard model w g =
    article [ class "goal-card", id ("goal-" ++ g.goal.id), tabindex -1 ]
        (goalSummary w g
            ++ [ note g.analysis.possibleCause
               , h3 [ class "form-heading" ] [ text "결과 보고" ]
               , formView model.forms (Report g.goal.id) "결과 보고" [ div [ class "fields" ] [ inputField model.forms (Report g.goal.id) "실측값" "value" "number" True, selectField model.forms (Report g.goal.id) "보고자" "reportedBy" True (peopleOptions w) ], inputField model.forms (Report g.goal.id) "결과 설명" "note" "text" True ]
               , if List.isEmpty g.results then
                    note "아직 결과가 없습니다."

                 else
                    div [ class "table-wrap" ] [ h3 [ class "form-heading" ] [ text "결과 추이 · 최근 순" ], table [] [ thead [] [ tr [] [ th [] [ text "기록 시각" ], th [] [ text "측정값" ], th [] [ text "설명" ] ] ], tbody [] (List.map (\r -> tr [] [ td [] [ text r.reportedAt ], td [] [ text (formatNumber r.value) ], td [] [ text r.note ] ]) g.results) ] ]
               , div [ class "actions" ]
                    [ button [ class "secondary", disabled (model.forms.busy || not model.forms.fresh), onClick (model.forms.submit (Evaluate g.goal.id)) ] [ text "평가 기록" ]
                    , button [ class "secondary", disabled model.forms.busy, onClick (model.goals ("goal-" ++ g.goal.id)) ] [ text "목표 관리 →" ]
                    ]
               ]
        )
