module MainTest exposing (suite)

import Application.TaskBoard as TaskBoard
import Domain.Task as Task
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "업무 상태"
        [ test "상태를 한국어 레이블로 표시한다" <|
            \_ ->
                Expect.equal
                    [ "초안", "검토 완료", "제출됨", "승인됨", "효력 발생" ]
                    (List.map Task.statusLabel Task.allStatuses)
        , test "긴급도와 중요도를 아이젠하워 사분면으로 변환한다" <|
            \_ ->
                Expect.equal
                    [ Task.DoFirst, Task.Schedule, Task.Delegate, Task.Eliminate ]
                    [ Task.quadrantOf Task.Urgent Task.Important
                    , Task.quadrantOf Task.NotUrgent Task.Important
                    , Task.quadrantOf Task.Urgent Task.NotImportant
                    , Task.quadrantOf Task.NotUrgent Task.NotImportant
                    ]
        , test "빈 제목 제출은 효과 없이 사용자에게 알린다" <|
            \_ ->
                case TaskBoard.update TaskBoard.SubmitTask TaskBoard.initialModel of
                    ( model, effects ) ->
                        Expect.equal
                            ( Just "업무 제목을 입력해 주세요.", [ TaskBoard.ClearNoticeAfter 1 ] )
                            ( model.notice, effects )
        , test "새 업무는 입력 상태와 관계없이 초안으로 저장한다" <|
            \_ ->
                let
                    baseModel =
                        TaskBoard.initialModel

                    model =
                        { baseModel
                            | loading = False
                            , draft =
                                { title = "신규 계약서 검토"
                                , description = "법무 검토를 위한 초안을 준비했습니다."
                                , status = Task.Effective
                                , urgency = Task.Urgent
                                , importance = Task.Important
                                , taskOwner = "김태스크"
                                , outcomeOwner = "이아웃컴"
                                , expectedResult = "검토 보고서"
                                }
                        }

                    ( updatedModel, effects ) =
                        TaskBoard.update TaskBoard.SubmitTask model
                in
                Expect.equal
                    ( True
                    , [ TaskBoard.CreateTask
                            { title = "신규 계약서 검토"
                            , description = "법무 검토를 위한 초안을 준비했습니다."
                            , status = Task.Draft
                            , urgency = Task.Urgent
                            , importance = Task.Important
                            , taskOwner = "김태스크"
                            , outcomeOwner = "이아웃컴"
                            , expectedResult = "검토 보고서"
                            }
                      ]
                    )
                    ( updatedModel.loading, effects )
        , test "카드를 다른 상태 컬럼에 놓으면 상태 변경 저장을 요청한다" <|
            \_ ->
                let
                    task =
                        { taskId = 1
                        , title = "신규 계약서 검토"
                        , description = "법무 검토를 위한 초안을 준비했습니다."
                        , status = Task.Draft
                        , urgency = Task.Urgent
                        , importance = Task.Important
                        , taskOwner = "김태스크"
                        , outcomeOwner = "이아웃컴"
                        , expectedResult = "검토 보고서"
                        , submittedResult = Nothing
                        , reviewComment = Nothing
                        }

                    baseModel =
                        TaskBoard.initialModel

                    initialDragModel =
                        { baseModel | tasks = [ task ], loading = False }

                    ( draggingModel, _ ) =
                        TaskBoard.update (TaskBoard.DragStarted task.taskId) initialDragModel

                    ( droppedModel, effects ) =
                        TaskBoard.update (TaskBoard.DroppedOn Task.Submitted) draggingModel
                in
                Expect.equal
                    ( True, [ TaskBoard.MoveTask 1 { title = task.title, description = task.description, status = Task.Submitted, urgency = task.urgency, importance = task.importance, taskOwner = task.taskOwner, outcomeOwner = task.outcomeOwner, expectedResult = task.expectedResult } ] )
                    ( droppedModel.loading, effects )
        , test "상태 변경 저장 후에는 받은 업무로 보드를 갱신한다" <|
            \_ ->
                let
                    originalTask =
                        { taskId = 1
                        , title = "신규 계약서 검토"
                        , description = "법무 검토를 위한 초안을 준비했습니다."
                        , status = Task.Draft
                        , urgency = Task.NotUrgent
                        , importance = Task.Important
                        , taskOwner = "김태스크"
                        , outcomeOwner = "이아웃컴"
                        , expectedResult = "검토 보고서"
                        , submittedResult = Nothing
                        , reviewComment = Nothing
                        }

                    movedTask =
                        { originalTask | status = Task.Submitted }

                    baseModel =
                        TaskBoard.initialModel

                    model =
                        { baseModel | tasks = [ originalTask ], loading = True }

                    ( updatedModel, effects ) =
                        TaskBoard.update (TaskBoard.MoveSaved (Ok movedTask)) model
                in
                Expect.equal
                    { tasks = [ movedTask ]
                    , loading = False
                    , notice = Just "업무 상태를 제출됨 상태로 변경했습니다."
                    , noticeVersion = 1
                    , effects = [ TaskBoard.ClearNoticeAfter 1 ]
                    }
                    { tasks = updatedModel.tasks
                    , loading = updatedModel.loading
                    , notice = updatedModel.notice
                    , noticeVersion = updatedModel.noticeVersion
                    , effects = effects
                    }
        , test "오래된 타이머는 새 알림을 닫지 않는다" <|
            \_ ->
                let
                    baseModel =
                        TaskBoard.initialModel

                    model =
                        { baseModel | notice = Just "새 알림", noticeVersion = 2 }

                    ( updatedModel, effects ) =
                        TaskBoard.update (TaskBoard.DismissNotice 1) model
                in
                Expect.equal ( Just "새 알림", [] ) ( updatedModel.notice, effects )
        ]
