module Page.Goals exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Form.Goal as Goal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as E
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias Controls msg =
    { draft : Goal.Draft, edit : Goal.Field -> String -> msg, forms : Config msg, expandedGoal : Maybe String, results : String -> msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, compiler : Compiler } -> Html msg
view model w =
    div []
        [ div [ class "metrics" ] (List.map (\( label_, amount, desc ) -> div [ class "metric" ] [ span [] [ text label_ ], strong [] [ text (String.fromInt amount) ], small [] [ text desc ] ]) [ ( "전체 목표", List.length w.goals, "측정 가능한 결과" ), ( "활성 목표", List.length (List.filter .active w.goals), "책임과 권한 검증 완료" ), ( "구조 진단", w.compiler.errors + w.compiler.warnings, "확인이 필요한 항목" ), ( "누적 학습", List.sum (List.map (.learnings >> List.length) w.reviews), "다음 결정의 근거" ) ])
        , div [ class "section-head" ] [ h2 [] [ text "목표 포트폴리오" ], a [ href "#new-goal" ] [ text "+ 목표 만들기" ] ]
        , if List.isEmpty w.goals then
            emptyState "어떤 결과를 만들고 싶나요?" "아래에서 측정 가능한 목표를 정의하고 책임자를 연결하세요."

          else
            div [ class "grid" ] (List.map (goalCard model w) w.goals)
        , details [ class "panel", id "new-goal" ] [ summary [] [ text "+ 목표 만들기" ], goalForm model w ]
        , diagnosticView w
        ]


goalForm : Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, compiler : Compiler } -> Html msg
goalForm model w =
    formView model.forms
        AddGoal
        "목표 초안 생성"
        [ note "초안 → 책임자 지정 → 권한 확인 → 활성화. 필요한 조건을 갖춘 뒤 실행합니다."
        , formInput model "어떤 결과를 만들고 싶나요?" Goal.Description "text" True
        , div [ class "fields" ] [ formInput model "KPI 이름" Goal.MetricName "text" True, formInput model "단위" Goal.Unit "text" True, formInput model "지표 식별자 · 같은 지표는 같은 ID" Goal.MetricId "text" True, formSelect model "좋은 결과의 방향" Goal.Direction True [ ( "HigherIsBetter", "높을수록 좋음" ), ( "LowerIsBetter", "낮을수록 좋음" ) ], formInput model "기준값" Goal.Baseline "number" True, formInput model "목표값" Goal.Target "number" True, formInput model "시작일 (UTC)" Goal.StartsAt "date" True, formInput model "마감일 (UTC)" Goal.Deadline "date" True, formInput model "필요 예산 (KRW)" Goal.Budget "number" True, formSelect model "상위 목표 (선택)" Goal.Parent False (( "", "없음" ) :: List.drop 1 (goalOptions w)) ]
        , checkValues (\key -> Goal.value model.draft (Goal.Permission key)) (\key -> model.edit (Goal.Permission key))
        ]


goalCard : Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, compiler : Compiler } -> GoalView -> Html msg
goalCard model w g =
    article [ class "goal-card", id ("goal-" ++ g.goal.id), tabindex -1 ]
        (goalSummary w g
            ++ [ div [ class "actions" ] [ button [ class "secondary", disabled model.forms.busy, onClick (model.results ("goal-" ++ g.goal.id)) ] [ text "결과 보고 · 평가 →" ] ]
               , details [ property "open" (E.bool (model.expandedGoal == Just g.goal.id)) ]
                    [ summary [] [ text "책임 · 권한 · 전략 관리" ]
                    , note g.analysis.possibleCause
                    , formView model.forms (Assign g.goal.id) "책임자 지정" [ selectField model.forms (Assign g.goal.id) "단일 최종 책임자" "owner" True (peopleOptions w) ]
                    , note "책임자 변경 또는 권한 부족 시 초안으로 돌아갑니다. 권한 메뉴에서 결정 권한을 조정하세요."
                    , div [ class "actions" ]
                        [ button [ disabled (model.forms.busy || not model.forms.fresh || g.active), onClick (model.forms.submit (Activate g.goal.id)) ]
                            [ text
                                (if g.active then
                                    "활성화됨"

                                 else
                                    "목표 활성화"
                                )
                            ]
                        ]
                    , formView model.forms (Strategy g.goal.id) "전략 변경 기록" [ inputField model.forms (Strategy g.goal.id) "새로운 전략과 변경 이유" "note" "text" True ]
                    , div [] (List.map (\( at, message ) -> note (String.left 10 at ++ " · " ++ message)) g.strategies)
                    ]
               ]
        )


formInput : Controls msg -> String -> Goal.Field -> String -> Bool -> Html msg
formInput model label_ field kind required_ =
    inputValue (Goal.fieldName field) (Goal.value model.draft field) (model.edit field) label_ kind required_


formSelect : Controls msg -> String -> Goal.Field -> Bool -> List ( String, String ) -> Html msg
formSelect model label_ field required_ options =
    selectValue (Goal.fieldName field) (Goal.value model.draft field) (model.edit field) label_ required_ options
