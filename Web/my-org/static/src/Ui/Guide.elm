module Ui.Guide exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page exposing (Page(..))
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias GuideStep =
    { done : Bool, title : String, instruction : String, page : Page, target : String }


view : { a | guideOpen : Bool, busy : Bool, toggle : msg, go : Page -> String -> msg } -> { b | goals : List GoalView, reviews : List Review, events : List Audit } -> Html msg
view model w =
    let
        goal key =
            List.filter (.goal >> .id >> (==) ("demo-" ++ key)) w.goals |> List.head

        active key =
            goal key |> Maybe.map .active |> Maybe.withDefault False

        assigned =
            goal "partners" |> Maybe.andThen .owner |> (/=) Nothing

        ready =
            goal "launch" |> Maybe.map (.analysis >> .coverage >> (==) 1) |> Maybe.withDefault False

        achieved =
            goal "revenue" |> Maybe.map (.evaluation >> .status >> (==) Achieved) |> Maybe.withDefault False

        evaluated =
            List.any (\e -> e.evaluatedGoal == Just "demo-revenue" && e.evaluatedStatus == Just Achieved) w.events

        reviewed =
            List.any (\r -> r.goal == "demo-revenue" && r.evaluation.status == Achieved && not (List.isEmpty r.learnings) && List.any (\d -> d.owner /= "" && d.deadline /= Nothing) r.decisions) w.reviews

        steps =
            [ GuideStep (active "partners")
                "01 · 빈 책임 자리 채우기"
                "파트너십 목표에 계약 권한을 가진 한유진을 최종 책임자로 지정하고 활성화하세요."
                (if assigned then
                    Dashboard

                 else
                    Responsibility
                )
                (if assigned then
                    "goal-demo-partners"

                 else
                    "owner-demo-partners"
                )
            , GuideStep (active "launch")
                "02 · 책임에 맞는 권한 주기"
                "이지원에게 채용 권한과 예산 30,000,000원을 부여하세요. 제품 출시 권한을 유지하고 신제품 출시 목표를 활성화하세요."
                (if ready then
                    Dashboard

                 else
                    Authorities
                )
                (if ready then
                    "goal-demo-launch"

                 else
                    "authority-demo-product"
                )
            , GuideStep (achieved && evaluated) "03 · 결과에서 평가까지" "매출 실측값 50 (단위: 억원)과 보고자, 설명을 보고한 뒤 평가 기록을 누르세요." Results "goal-demo-revenue"
            , GuideStep reviewed "04 · 배움을 다음 결정으로" "매출 목표의 학습과 다음 결정, 담당자, 미래 기한을 기록하세요. 달성 결과와 평가가 함께 보존됩니다." Reviews "review-form"
            , GuideStep designed "05 · 업무에서 에이전트 설계로" "저장된 업무 흐름 4건에서 도출한 역할 후보를 설계안으로 가져오고, 등급과 승인 주체, 인계 대상을 검토해 저장하세요. 저장하면 구조 화면과 내보내기를 사용할 수 있습니다." AgentDrafts "agent-import"
            ]

        designed =
            List.any (\e -> e.activity.tag == "AgentRolesSaved") w.events

        count =
            List.length (List.filter .done steps)
    in
    section [ class "demo-guide" ]
        [ div [ class "demo-heading" ] [ div [] [ span [ class "tag" ] [ text "DEMO · 가상 데이터" ], h2 [] [ text "조직의 운영 흐름, 다섯 단계로 체험하세요" ], p [] [ text "6명 · 7개 목표 · 5가지 성과 상태 · 업무 흐름 4건. 실제 저장 상태로 진행률을 계산합니다." ] ], span [ class "guide-count" ] [ text (String.fromInt count ++ " / 5 완료") ] ]
        , button
            [ class "guide-toggle secondary"
            , onClick model.toggle
            , attribute "aria-expanded"
                (if model.guideOpen then
                    "true"

                 else
                    "false"
                )
            ]
            [ text
                (if model.guideOpen then
                    "체험 가이드 접기"

                 else
                    "체험 가이드 열기"
                )
            ]
        , if model.guideOpen then
            div []
                [ div [ class "guide-steps" ]
                    (List.map
                        (\step_ ->
                            article [ classList [ ( "guide-step", True ), ( "complete", step_.done ) ] ]
                                [ span [ class "step-state" ]
                                    [ text
                                        (if step_.done then
                                            "✓ 완료"

                                         else
                                            "○ 체험 대기"
                                        )
                                    ]
                                , h3 [] [ text step_.title ]
                                , p [] [ text step_.instruction ]
                                , button [ class "secondary", disabled model.busy, onClick (model.go step_.page step_.target) ]
                                    [ text
                                        (if step_.done then
                                            "다시 살펴보기 →"

                                         else
                                            "이 단계 진행 →"
                                        )
                                    ]
                                ]
                        )
                        steps
                    )
                , note "전사 성장 지수는 하위 목표의 자동 합계가 아닌 별도 보고 KPI입니다. 초기 진단과 결과 샘플은 의도한 가상 체험 사례입니다. 감사 시각은 실제 가져온 시각입니다."
                , div [ class "actions" ] [ button [ class "secondary", disabled model.busy, onClick (model.go People "new-person") ] [ text "구성원 관리 →" ], button [ class "secondary", disabled model.busy, onClick (model.go Responsibility "responsibility-graph") ] [ text "관계 그래프 →" ], button [ class "secondary", disabled model.busy, onClick (model.go ActivityLog "audit-history") ] [ text "활동 기록 →" ] ]
                ]

          else
            text ""
        ]
