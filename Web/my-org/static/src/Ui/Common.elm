module Ui.Common exposing (..)

import Domain exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Ui.Label exposing (..)


panel : String -> List (Html msg) -> Html msg
panel title children =
    section [ class "panel" ] (h2 [] [ text title ] :: children)


note : String -> Html msg
note content =
    p [ class "note" ] [ text content ]


emptyState : String -> String -> Html msg
emptyState title content =
    section [ class "panel empty" ] [ h2 [] [ text title ], p [] [ text content ] ]


badge : GoalView -> Html msg
badge g =
    span [ classList [ ( "tag", True ), ( "draft", not g.active ), ( "error", g.evaluation.status == OffTrack ), ( "warn", g.evaluation.status == AtRisk ) ] ]
        [ text
            (if g.active then
                statusName g.evaluation.status

             else
                "초안"
            )
        ]


goalSummary : { a | people : List Person } -> GoalView -> List (Html msg)
goalSummary w g =
    [ badge g
    , h2 [] [ text g.goal.description ]
    , small []
        [ text
            (g.goal.metric.name
                ++ " · "
                ++ (if g.goal.metric.direction == "HigherIsBetter" then
                        "↑ 증가"

                    else
                        "↓ 감소"
                   )
                ++ " 목표"
            )
        ]
    , div [ class "goal-values" ] [ strong [] [ text (g.evaluation.latestValue |> Maybe.map formatNumber |> Maybe.withDefault "—") ], span [ class "muted" ] [ text ("/ " ++ formatNumber g.goal.target ++ " " ++ g.goal.metric.unit) ] ]
    , progress [ Html.Attributes.max "1", value (String.fromFloat (clamp 0 1 g.evaluation.progress)), attribute "aria-label" "목표 달성률" ] []
    , small [] [ text (String.fromInt (round (g.evaluation.progress * 100)) ++ "% 달성 · 기준 " ++ formatNumber g.goal.baseline) ]
    , div [ class "meta" ] [ span [] [ text (g.owner |> Maybe.map (personName w) |> Maybe.withDefault "책임자 미지정") ], span [] [ text (String.left 10 g.goal.deadline ++ " 마감") ] ]
    ]


diagnosticView : { a | compiler : Compiler } -> Html msg
diagnosticView w =
    panel "조직 구조 검사"
        [ div [ class "section-head" ] [ text "다음 행동을 위한 피드백", span [ class "tag warn" ] [ text (String.fromInt w.compiler.errors ++ " 오류 · " ++ String.fromInt w.compiler.warnings ++ " 경고") ] ]
        , if List.isEmpty w.compiler.diagnostics then
            note "구조 검사를 통과했습니다. 결과를 보고하고 학습을 이어가세요."

          else
            div [] (List.map (\d -> div [ classList [ ( "diagnostic", True ), ( "error", d.severity == "Error" ) ] ] [ code [] [ text d.code ], strong [] [ text d.message ], p [] [ text d.subject ], div [] (List.map (\line -> p [] [ text line ]) d.details) ]) w.compiler.diagnostics)
        , note "권한 집중도는 권한 종류와 예산 보유를 각각 1점으로 세는 규칙 기반 추정치입니다."
        ]
