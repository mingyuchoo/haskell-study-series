module Presentation.TaskBoard exposing (view)

import Application.TaskBoard exposing (Model, Msg(..))
import Domain.Task as Task exposing (Importance, Status, Task, Urgency)
import Html exposing (Html, button, div, h1, h2, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (attribute, checked, class, disabled, for, id, name, placeholder, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode


view : Model -> Html Msg
view model =
    div [ class "page-shell" ]
        [ toastView model
        , headerView model
        , div [ class "content" ]
            [ formView model
            , kanbanBoard model
            ]
        ]


headerView : Model -> Html Msg
headerView model =
    div [ class "hero" ]
        [ div [ class "brand-row" ]
            [ div [ class "brand-mark" ] [ text "GS" ]
            , div [] [ span [ class "eyebrow" ] [ text "GENERAL SERVICE" ], h1 [] [ text "업무 관리" ] ]
            , span [ class "live-dot" ] [ text "관리자 모드" ]
            ]
        , p [ class "hero-copy" ] [ text "업무의 흐름을 한눈에 보고, 다음 단계로 자연스럽게 이어가세요." ]
        ]


toastView : Model -> Html Msg
toastView model =
    case model.notice of
        Just notice ->
            div [ class "notice", attribute "aria-live" "polite", attribute "role" "status" ] [ text notice ]

        Nothing ->
            text ""


formView : Model -> Html Msg
formView model =
    let
        isEditing =
            model.editing /= Nothing
    in
    div [ class "card editor" ]
        [ div [ class "section-heading" ]
            [ h2 []
                [ text
                    (if isEditing then
                        "업무 수정"

                     else
                        "새 업무 등록"
                    )
                ]
            , span []
                [ text
                    (if isEditing then
                        "변경 내용을 저장하세요"

                     else
                        "업무를 흐름에 추가하세요"
                    )
                ]
            ]
        , div [ class "form-grid" ]
            [ div
                [ class
                    ("field title-field"
                        ++ (if isEditing then
                                ""

                            else
                                " full-width"
                           )
                    )
                ]
                [ label [ for "title" ] [ text "업무 제목" ]
                , input [ id "title", value model.draft.title, placeholder "예: 2026년 4분기 계약 갱신", onInput EditTitle ] []
                ]
            , if isEditing then
                div [ class "field" ]
                    [ label [ for "status" ] [ text "현재 상태" ]
                    , select [ id "status", onInput EditStatus ] (List.map (statusOption model.draft.status) Task.allStatuses)
                    ]

              else
                text ""
            , radioField
                "urgency"
                "긴급도"
                model.draft.urgency
                [ ( Task.Urgent, "긴급" ), ( Task.NotUrgent, "긴급하지 않음" ) ]
                Task.urgencyString
                EditUrgency
            , radioField
                "importance"
                "중요도"
                model.draft.importance
                [ ( Task.Important, "중요" ), ( Task.NotImportant, "중요하지 않음" ) ]
                Task.importanceString
                EditImportance
            , div [ class "field" ]
                [ label [ for "task-owner" ] [ text "Task Owner" ]
                , input [ id "task-owner", value model.draft.taskOwner, placeholder "결과물을 제출할 담당자", onInput EditTaskOwner ] []
                ]
            , div [ class "field" ]
                [ label [ for "outcome-owner" ] [ text "Outcome Owner" ]
                , input [ id "outcome-owner", value model.draft.outcomeOwner, placeholder "결과물을 리뷰·승인할 담당자", onInput EditOutcomeOwner ] []
                ]
            , div [ class "field description-field" ]
                [ label [ for "expected-result" ] [ text "기대 결과물" ]
                , textarea [ id "expected-result", value model.draft.expectedResult, placeholder "Outcome Owner가 원하는 결과물을 정의하세요.", onInput EditExpectedResult ] []
                ]
            , div [ class "field description-field" ]
                [ label [ for "description" ] [ text "설명" ]
                , textarea [ id "description", value model.draft.description, placeholder "관리자 메모 또는 업무의 다음 단계를 적어주세요.", onInput EditDescription ] []
                ]
            ]
        , div [ class "actions" ]
            [ if isEditing then
                button [ class "button secondary", onClick CancelEdit ] [ text "취소" ]

              else
                text ""
            , button [ class "button primary", disabled model.loading, onClick SubmitTask ]
                [ text
                    (if isEditing then
                        "변경 저장"

                     else
                        "업무 등록"
                    )
                ]
            ]
        ]


radioField fieldName fieldLabel selectedValue choices toString toMessage =
    div [ class "field radio-field" ]
        [ span [ class "radio-label" ] [ text fieldLabel ]
        , div [ class "radio-group", attribute "role" "radiogroup", attribute "aria-label" fieldLabel ]
            (List.map
                (\( choice, choiceLabel ) ->
                    label [ class "radio-option" ]
                        [ input
                            [ type_ "radio"
                            , name fieldName
                            , value (toString choice)
                            , checked (selectedValue == choice)
                            , onClick (toMessage (toString choice))
                            ]
                            []
                        , span [] [ text choiceLabel ]
                        ]
                )
                choices
            )
        ]


kanbanBoard : Model -> Html Msg
kanbanBoard model =
    div [ class "kanban-section" ]
        [ div [ class "board-heading" ]
            [ div []
                [ h2 [] [ text "업무 보드" ]
                , p [] [ text "카드를 원하는 상태 컬럼으로 끌어 옮겨 다음 단계를 관리하세요." ]
                ]
            , span [ class "board-total" ] [ text (String.fromInt (List.length model.tasks) ++ "개 업무") ]
            ]
        , prioritySummary model.tasks
        , if model.loading && List.isEmpty model.tasks then
            p [ class "empty board-empty" ] [ text "업무를 불러오는 중입니다…" ]

          else
            div [ class "kanban-board" ] (List.map (kanbanColumn model) Task.allStatuses)
        ]


kanbanColumn : Model -> Status -> Html Msg
kanbanColumn model taskStatus =
    let
        statusTasks =
            List.filter (\task -> task.status == taskStatus) model.tasks

        dropTargetClass =
            if model.dropTarget == Just taskStatus then
                " is-drop-target"

            else
                ""
    in
    div
        [ class ("kanban-column " ++ Task.statusClass taskStatus ++ dropTargetClass)
        , preventDefaultOn "dragover" (Decode.succeed ( DragOver taskStatus, True ))
        , preventDefaultOn "drop" (Decode.succeed ( DroppedOn taskStatus, True ))
        ]
        [ div [ class "column-heading" ]
            [ span [ class "column-title" ] [ text (Task.statusLabel taskStatus) ]
            , span [ class "column-count" ] [ text (String.fromInt (List.length statusTasks)) ]
            ]
        , div [ class "kanban-cards" ]
            (if List.isEmpty statusTasks then
                [ div [ class "column-empty" ] [ text "등록된 업무가 없습니다" ] ]

             else
                List.map (taskCard model) statusTasks
            )
        ]


taskCard : Model -> Task -> Html Msg
taskCard model task =
    div
        [ class
            ("task-card"
                ++ (if model.draggedTaskId == Just task.taskId then
                        " is-dragging"

                    else
                        ""
                   )
            )
        , attribute "draggable"
            (if model.loading then
                "false"

             else
                "true"
            )
        , on "dragstart" (Decode.succeed (DragStarted task.taskId))
        , on "dragend" (Decode.succeed DragEnded)
        ]
        [ h2 [] [ text task.title ]
        , span [ class ("priority-badge " ++ Task.quadrantClass (Task.quadrantOf task.urgency task.importance)) ]
            [ text (Task.quadrantLabel (Task.quadrantOf task.urgency task.importance)) ]
        , p [ class "task-description" ]
            [ text
                (if task.description == "" then
                    "등록된 설명이 없습니다."

                 else
                    task.description
                )
            ]
        , div [ class "result-flow" ]
            [ p [] [ text ("Task Owner: " ++ task.taskOwner) ]
            , p [] [ text ("Outcome Owner: " ++ task.outcomeOwner) ]
            , p [] [ text ("기대 결과물: " ++ task.expectedResult) ]
            , p [] [ text ("제출 결과물: " ++ Maybe.withDefault "아직 제출되지 않았습니다." task.submittedResult) ]
            , p [] [ text ("리뷰: " ++ Maybe.withDefault "리뷰 대기" task.reviewComment) ]
            ]
        , div [ class "card-footer" ]
            [ span [ class "task-id" ] [ text ("업무 #" ++ String.fromInt task.taskId) ]
            , div [ class "row-actions" ]
                [ button [ class "text-button", disabled model.loading, onClick (StartEdit task) ] [ text "수정" ]
                , button [ class "text-button danger", disabled model.loading, onClick (DeleteRequested task.taskId) ] [ text "삭제" ]
                ]
            ]
        ]


statusOption : Status -> Status -> Html Msg
statusOption selectedStatus current =
    option [ value (Task.statusString current), selected (selectedStatus == current) ] [ text (Task.statusLabel current) ]


urgencyOption : Urgency -> Urgency -> Html Msg
urgencyOption selectedUrgency current =
    option [ value (Task.urgencyString current), selected (selectedUrgency == current) ] [ text (Task.urgencyLabel current) ]


importanceOption : Importance -> Importance -> Html Msg
importanceOption selectedImportance current =
    option [ value (Task.importanceString current), selected (selectedImportance == current) ] [ text (Task.importanceLabel current) ]


prioritySummary : List Task -> Html Msg
prioritySummary tasks =
    let
        summaryItem quadrant =
            let
                count =
                    tasks
                        |> List.filter (\task -> Task.quadrantOf task.urgency task.importance == quadrant)
                        |> List.length
            in
            div [ class ("priority-summary-item " ++ Task.quadrantClass quadrant) ]
                [ span [ class "priority-summary-label" ] [ text (Task.quadrantLabel quadrant) ]
                , span [ class "priority-summary-count" ] [ text (String.fromInt count) ]
                ]
    in
    div [ class "priority-summary", attribute "aria-label" "아이젠하워 매트릭스 우선순위 요약" ]
        [ summaryItem Task.DoFirst
        , summaryItem Task.Schedule
        , summaryItem Task.Delegate
        , summaryItem Task.Eliminate
        ]
