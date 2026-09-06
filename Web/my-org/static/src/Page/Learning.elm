module Page.Learning exposing (view)

import Domain exposing (..)
import Form.Action exposing (..)
import Form.Review as Review
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui.Common exposing (..)
import Ui.Form exposing (..)
import Ui.Label exposing (..)


type alias Controls msg =
    { draft : Review.Draft, edit : Review.Field -> String -> msg, forms : Config msg }


view : Controls msg -> { a | goals : List GoalView, people : List Person, reviews : List Review, reviewWarnings : List ReviewWarning, events : List Audit } -> Html msg
view model w =
    div []
        [ section [ class "panel", id "review-form", tabindex -1 ] [ h2 [] [ text "회고와 다음 결정 기록" ], formView model.forms AddReview "회고 기록" [ formSelect model "회고할 목표" Review.Goal True (goalOptions w), formInput model "회고 요약" Review.Note "text" True, label [] [ text "새롭게 배운 점 (선택)", textarea [ value (Review.value model.draft Review.Learning), onInput (model.edit Review.Learning) ] [] ], formInput model "다음 결정 (선택)" Review.Decision "text" False, div [ class "fields" ] [ formSelect model "결정 담당자" Review.DecisionOwner False (peopleOptions w), formInput model "결정 기한 (UTC, 선택)" Review.DecisionDeadline "date" False ], note "현재 최신 결과와 평가가 함께 보존됩니다. 결정과 학습이 모두 없으면 구조 검사가 경고합니다." ] ]
        , div [ class "grid" ]
            (List.map
                (\r ->
                    section [ class "panel" ]
                        [ span [ class "tag" ] [ text (String.left 10 r.heldAt ++ " · " ++ statusName r.evaluation.status) ]
                        , h2 [ class "form-heading" ] [ text (goalName w r.goal) ]
                        , p [] [ text r.note ]
                        , h3 [] [ text "학습" ]
                        , if List.isEmpty r.learnings then
                            note "기록된 학습 없음"

                          else
                            div [] (List.map (\learning -> p [] [ text learning ]) r.learnings)
                        , h3 [] [ text "다음 결정" ]
                        , if List.isEmpty r.decisions then
                            note "기록된 결정 없음"

                          else
                            div [] (List.map (\d -> p [] [ text d.text, br [] [], small [] [ text (personName w d.owner ++ " · " ++ (d.deadline |> Maybe.map (String.left 10) |> Maybe.withDefault "기한 미정")) ] ]) r.decisions)
                        , div [] (w.reviewWarnings |> List.filter (.id >> (==) r.id) |> List.concatMap .warnings |> List.map (\warning -> p [ class "tag warn" ] [ text warning ]))
                        ]
                )
                w.reviews
            )
        , section [ class "panel", id "audit-history", tabindex -1 ]
            [ h2 [] [ text "조직의 의사결정 기록" ]
            , note "서버가 시각과 순번을 부여합니다. 행위자는 요청의 기록 주체이며 인증된 신원 증명이 아닙니다."
            , if List.isEmpty w.events then
                note "아직 기록이 없습니다."

              else
                div [] (List.map (\event -> div [ class "event" ] [ small [] [ text ("#" ++ String.fromInt event.seq ++ " · " ++ event.at), br [] [], text (event.actor |> Maybe.map (personName w) |> Maybe.withDefault "로컬 운영자 (미인증)") ], p [] [ text event.description ] ]) w.events)
            ]
        ]


formInput : Controls msg -> String -> Review.Field -> String -> Bool -> Html msg
formInput model label_ field kind required_ =
    inputValue (Review.fieldName field) (Review.value model.draft field) (model.edit field) label_ kind required_


formSelect : Controls msg -> String -> Review.Field -> Bool -> List ( String, String ) -> Html msg
formSelect model label_ field required_ options =
    selectValue (Review.fieldName field) (Review.value model.draft field) (model.edit field) label_ required_ options
