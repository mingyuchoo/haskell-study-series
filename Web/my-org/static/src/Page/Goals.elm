module Page.Goals exposing (view, viewWith)

import Dict
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
import Ui.ListView as ListView exposing (Mode(..))


type alias Controls msg =
    { draft : Goal.Draft, edit : Goal.Field -> String -> msg, forms : Config msg, expandedGoal : Maybe String, results : String -> msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, compiler : Compiler } -> Html msg
view =
    viewWith Table


viewWith : Mode -> Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, compiler : Compiler } -> Html msg
viewWith mode model w =
    div []
        [ div [ class "metrics" ] (List.map (\( label_, amount, desc ) -> div [ class "metric" ] [ span [] [ text label_ ], strong [] [ text (String.fromInt amount) ], small [] [ text desc ] ]) [ ( "전체 목표", List.length w.goals, "측정 가능한 결과" ), ( "활성 목표", List.length (List.filter .active w.goals), "책임과 권한 검증 완료" ), ( "구조 진단", w.compiler.errors + w.compiler.warnings, "확인이 필요한 항목" ), ( "누적 학습", List.sum (List.map (.learnings >> List.length) w.reviews), "다음 결정의 근거" ) ])
        , div [ class "section-head" ] [ h2 [] [ text "목표 포트폴리오" ], a [ href "#new-goal" ] [ text "+ 목표 만들기" ] ]
        , if List.isEmpty w.goals then
            emptyState "현재 관리 중인 목표가 있나요?" "확인된 측정 기준이 있다면 아래에서 목표 초안을 만드세요. 모르는 내용은 조직 진단에 미확인으로 남길 수 있습니다."

          else if mode == Table then
            goalTable model w

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
        [ note "현재 관리 중인 목표를 정리하는 운영 화면입니다. 아직 목표나 측정 기준을 모른다면 조직 진단에 미확인으로 남기고 나중에 입력하세요. 초안 생성 후 책임·권한을 확인하여 활성화합니다."
        , fieldset [ class "form-section" ]
            [ legend [] [ text "1 · 책임져야 하는 결과" ]
            , formInput model "현재 관리 중인 목표 / 결과" Goal.Description "text" True
            , formSelect model "상위 목표 (선택)" Goal.Parent False (( "", "없음" ) :: List.drop 1 (goalOptions w))
            ]
        , fieldset [ class "form-section" ]
            [ legend [] [ text "2 · 결과를 확인하는 측정 기준" ]
            , metricPicker model w
            , div [ class "fields" ] [ formInput model "기준값" Goal.Baseline "number" True, formInput model "목표값" Goal.Target "number" True, formInput model "시작일 (UTC)" Goal.StartsAt "date" True, formInput model "마감일 (UTC)" Goal.Deadline "date" True ]
            ]
        , fieldset [ class "form-section" ]
            [ legend [] [ text "3 · 목표 실행에 필요한 조건" ]
            , note "이 목표에 필요한 권한과 예산입니다. 현재 책임자가 보유한 권한은 권한 화면에서 별도로 기록합니다. 확인되지 않은 조건을 0이나 권한 없음으로 대신 입력하지 마세요."
            , formInput model "필요 예산 (KRW)" Goal.Budget "number" True
            , checkValues (\key -> Goal.value model.draft (Goal.Permission key)) (\key -> model.edit (Goal.Permission key))
            ]
        ]


goalCard : Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, compiler : Compiler } -> GoalView -> Html msg
goalCard model w g =
    article [ class "goal-card", id ("goal-" ++ g.goal.id), tabindex -1 ]
        (goalContent model w g)


goalContent model w g =
    goalSummary w g
        ++ [ resultLink model g
           , details [ property "open" (E.bool (model.expandedGoal == Just g.goal.id)) ]
                (summary [] [ text "책임 · 권한 · 전략 관리" ] :: goalManagement model w g)
           ]


resultLink model g =
    div [ class "actions" ] [ button [ class "secondary", disabled model.forms.busy, onClick (model.results ("goal-" ++ g.goal.id)) ] [ text "결과 보고 · 평가 →" ] ]


goalManagement model w g =
    [ note g.analysis.possibleCause
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


formInput : Controls msg -> String -> Goal.Field -> String -> Bool -> Html msg
formInput model label_ field kind required_ =
    inputValue (Goal.fieldName field) (Goal.value model.draft field) (model.edit field) label_ kind required_


formSelect : Controls msg -> String -> Goal.Field -> Bool -> List ( String, String ) -> Html msg
formSelect model label_ field required_ options =
    selectValue (Goal.fieldName field) (Goal.value model.draft field) (model.edit field) label_ required_ options


goalTable model w =
    ListView.tableView "목표 포트폴리오"
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
                , ListView.detailRow 6 [ property "open" (E.bool (model.expandedGoal == Just g.goal.id)) ] (g.goal.description ++ " · 책임 · 권한 · 전략 관리") (resultLink model g :: goalManagement model w g)
                ]
            )
            w.goals
        )


metricPicker model w =
    let
        metrics =
            w.goals |> List.map (.goal >> .metric) |> List.map (\metric -> ( metric.id, metric )) |> Dict.fromList

        selected =
            Dict.get model.draft.metricId metrics
    in
    div []
        [ selectValue "goal-metric-choice" (selected |> Maybe.map .id |> Maybe.withDefault "") (model.edit Goal.MetricId) "사용할 지표" False (( "", "새 지표 만들기 · ID 자동 생성" ) :: (Dict.values metrics |> List.map (\metric -> ( metric.id, metric.name ++ " · " ++ metric.unit ))))
        , note "같은 지표를 공유하는 목표는 기존 지표를 선택하세요. 동일 지표의 책임 관계를 연결하는 데 사용합니다. 이름이 같아도 정의가 다르면 새 지표를 만드세요."
        , case selected of
            Just metric ->
                note
                    ("선택한 지표: "
                        ++ metric.name
                        ++ " / "
                        ++ metric.unit
                        ++ " / "
                        ++ (if metric.direction == "HigherIsBetter" then
                                "높을수록 좋음"

                            else
                                "낮을수록 좋음"
                           )
                    )

            Nothing ->
                div [ class "fields" ] [ formInput model "KPI 이름" Goal.MetricName "text" True, formInput model "단위" Goal.Unit "text" True, formSelect model "좋은 결과의 방향" Goal.Direction True [ ( "HigherIsBetter", "높을수록 좋음" ), ( "LowerIsBetter", "낮을수록 좋음" ) ] ]
        ]
