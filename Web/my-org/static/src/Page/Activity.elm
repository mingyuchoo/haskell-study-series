module Page.Activity exposing (view)

import Domain exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Ui.Activity as Activity
import Ui.Common exposing (note)
import Ui.ListView as ListView exposing (Mode(..))


view : Mode -> Activity.State -> (Activity.State -> msg) -> Workspace -> Html msg
view mode state change w =
    let
        events =
            Activity.filtered state w

        field label_ kind val update =
            label [] [ text label_, input [ type_ kind, value val, onInput update ] [] ]
    in
    section [ class "panel", id "audit-history", tabindex -1 ]
        [ h2 [] [ text "조직 활동 기록" ]
        , note "조직 전체의 변경 이력입니다. 기록 주체는 요청에 기록된 값이며 인증된 신원 증명이 아닙니다. 이름은 현재 정보로 표시하며 원본 ID와 기록 데이터는 상세에서 확인할 수 있습니다."
        , div [ class "fields activity-filters" ]
            [ field "활동 검색" "search" state.query (\v -> change { state | query = v })
            , label []
                [ text "변경 유형"
                , select [ value state.kind, onInput (\v -> change { state | kind = v }) ]
                    (List.map
                        (\v ->
                            option [ value v ]
                                [ text
                                    (if v == "" then
                                        "전체 유형"

                                     else
                                        v
                                    )
                                ]
                        )
                        [ "", "조직", "구성원", "목표", "책임", "권한", "결과", "학습", "기타" ]
                    )
                ]
            , field "시작일 (UTC)" "date" state.from (\v -> change { state | from = v })
            , field "종료일 (UTC)" "date" state.until (\v -> change { state | until = v })
            ]
        , if state.from /= "" && state.until /= "" && state.from > state.until then
            p [ attribute "role" "alert", class "error" ] [ text "종료일은 시작일 이후로 선택하세요." ]

          else
            text ""
        , div [ class "actions" ] [ p [ attribute "role" "status" ] [ text ("검색 결과 " ++ String.fromInt (List.length events) ++ "건" ++ (state.review |> Maybe.map (\_ -> " · 선택한 회고의 관련 기록") |> Maybe.withDefault "")) ], button [ type_ "button", class "secondary", onClick (change Activity.init) ] [ text "필터 초기화" ] ]
        , if List.isEmpty events then
            note "조건에 맞는 활동 기록이 없습니다."

          else if mode == Table then
            ListView.tableView "조직 활동 기록" [ "시각 (UTC)", "변경 유형", "대상", "기록 주체", "내용" ] (List.map (row w) events)

          else
            div [ class "grid" ] (List.map (card w) events)
        ]


row w event =
    tr []
        [ td [] [ text (Activity.timestamp event.at) ]
        , td [] [ text (Activity.category event) ]
        , th [ scope "row" ] [ text (Activity.targetName w event) ]
        , td [] [ text (Activity.actorName w event) ]
        , td [] [ p [] [ text (Activity.description w event) ], detail event ]
        ]


card w event =
    article [ class "panel" ]
        [ span [ class "tag" ] [ text (Activity.category event) ]
        , h3 [] [ text (Activity.targetName w event) ]
        , p [] [ text (Activity.timestamp event.at) ]
        , p [] [ text (Activity.actorName w event) ]
        , p [] [ text (Activity.description w event) ]
        , detail event
        ]


detail event =
    details []
        [ summary [] [ text ("기록 상세 #" ++ String.fromInt event.seq) ]
        , p [] [ text ("원본 시각: " ++ event.at) ]
        , p [] [ text ("기록 주체 ID: " ++ Maybe.withDefault "없음" event.actor) ]
        , p [] [ text ("대상 ID: " ++ event.activity.targetId) ]
        , p [] [ text ("원본 설명: " ++ event.description) ]
        , pre [ class "activity-raw" ] [ code [] [ text event.activity.raw ] ]
        ]
