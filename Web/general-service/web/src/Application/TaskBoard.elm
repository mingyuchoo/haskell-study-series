module Application.TaskBoard exposing
    ( ApiError(..)
    , Effect(..)
    , Model
    , Msg(..)
    , init
    , initialModel
    , update
    )

import Domain.Task as Task exposing (Task, TaskInput)


type alias Model =
    { tasks : List Task
    , draft : TaskInput
    , editing : Maybe Task
    , draggedTaskId : Maybe Int
    , dropTarget : Maybe Task.Status
    , loading : Bool
    , notice : Maybe String
    , noticeVersion : Int
    }


type ApiError
    = RequestFailed


type Effect
    = LoadTasks
    | CreateTask TaskInput
    | UpdateTask Int TaskInput
    | MoveTask Int TaskInput
    | DeleteTask Int
    | ClearNoticeAfter Int


type Msg
    = GotTasks (Result ApiError (List Task))
    | EditTitle String
    | EditDescription String
    | EditStatus String
    | EditUrgency String
    | EditImportance String
    | EditTaskOwner String
    | EditOutcomeOwner String
    | EditExpectedResult String
    | SubmitTask
    | StartEdit Task
    | CancelEdit
    | DeleteRequested Int
    | Saved (Result ApiError Task)
    | Deleted (Result ApiError ())
    | DragStarted Int
    | DragOver Task.Status
    | DragEnded
    | DroppedOn Task.Status
    | MoveSaved (Result ApiError Task)
    | DismissNotice Int


initialModel : Model
initialModel =
    { tasks = []
    , draft = Task.emptyInput
    , editing = Nothing
    , draggedTaskId = Nothing
    , dropTarget = Nothing
    , loading = True
    , notice = Nothing
    , noticeVersion = 0
    }


init : ( Model, List Effect )
init =
    ( initialModel, [ LoadTasks ] )


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        GotTasks result ->
            case result of
                Ok tasks ->
                    ( { model | tasks = tasks, loading = False }, [] )

                Err RequestFailed ->
                    showNotice "업무 목록을 불러오지 못했습니다." { model | loading = False }

        EditTitle title ->
            ( updateForm (\form -> { form | title = title }) model, [] )

        EditDescription description ->
            ( updateForm (\form -> { form | description = description }) model, [] )

        EditStatus rawStatus ->
            case Task.statusFromString rawStatus of
                Just taskStatus ->
                    ( updateForm (\form -> { form | status = taskStatus }) model, [] )

                Nothing ->
                    showNotice "알 수 없는 업무 상태입니다." model

        EditUrgency rawUrgency ->
            case Task.urgencyFromString rawUrgency of
                Just taskUrgency ->
                    ( updateForm (\form -> { form | urgency = taskUrgency }) model, [] )

                Nothing ->
                    showNotice "알 수 없는 긴급도입니다." model

        EditImportance rawImportance ->
            case Task.importanceFromString rawImportance of
                Just taskImportance ->
                    ( updateForm (\form -> { form | importance = taskImportance }) model, [] )

                Nothing ->
                    showNotice "알 수 없는 중요도입니다." model

        EditTaskOwner owner ->
            ( updateForm (\form -> { form | taskOwner = owner }) model, [] )

        EditOutcomeOwner owner ->
            ( updateForm (\form -> { form | outcomeOwner = owner }) model, [] )

        EditExpectedResult expected ->
            ( updateForm (\form -> { form | expectedResult = expected }) model, [] )

        SubmitTask ->
            case Task.validateInput model.draft of
                Err Task.TitleRequired ->
                    showNotice "업무 제목을 입력해 주세요." model

                Ok input ->
                    case model.editing of
                        Just task ->
                            ( { model | loading = True, notice = Nothing }, [ UpdateTask task.taskId input ] )

                        Nothing ->
                            ( { model | loading = True, notice = Nothing }
                            , [ CreateTask { input | status = Task.Draft } ]
                            )

        StartEdit task ->
            ( { model | editing = Just task, draft = toInput task, notice = Nothing }, [] )

        CancelEdit ->
            ( { model | editing = Nothing, draft = Task.emptyInput }, [] )

        DeleteRequested taskId ->
            ( { model | loading = True, notice = Nothing }, [ DeleteTask taskId ] )

        Saved result ->
            case result of
                Ok _ ->
                    showNotice "업무가 저장되었습니다." { model | draft = Task.emptyInput, editing = Nothing }
                        |> addEffect LoadTasks

                Err RequestFailed ->
                    showNotice "저장하지 못했습니다. 다시 시도해 주세요." { model | loading = False }

        Deleted result ->
            case result of
                Ok _ ->
                    showNotice "업무를 삭제했습니다." model
                        |> addEffect LoadTasks

                Err RequestFailed ->
                    showNotice "업무를 삭제하지 못했습니다." { model | loading = False }

        DragStarted taskId ->
            if model.loading then
                ( model, [] )

            else
                ( { model | draggedTaskId = Just taskId, dropTarget = Nothing, notice = Nothing }, [] )

        DragOver taskStatus ->
            case model.draggedTaskId of
                Just _ ->
                    ( { model | dropTarget = Just taskStatus }, [] )

                Nothing ->
                    ( model, [] )

        DragEnded ->
            ( { model | draggedTaskId = Nothing, dropTarget = Nothing }, [] )

        DroppedOn targetStatus ->
            case ( model.loading, model.draggedTaskId ) of
                ( False, Just taskId ) ->
                    case List.filter (\task -> task.taskId == taskId) model.tasks |> List.head of
                        Just task ->
                            if task.status == targetStatus then
                                ( { model | draggedTaskId = Nothing, dropTarget = Nothing }, [] )

                            else
                                ( { model | loading = True, draggedTaskId = Nothing, dropTarget = Nothing, notice = Nothing }
                                , [ MoveTask taskId { title = task.title, description = task.description, status = targetStatus, urgency = task.urgency, importance = task.importance, taskOwner = task.taskOwner, outcomeOwner = task.outcomeOwner, expectedResult = task.expectedResult } ]
                                )

                        Nothing ->
                            ( { model | draggedTaskId = Nothing, dropTarget = Nothing }, [] )

                _ ->
                    ( model, [] )

        MoveSaved result ->
            case result of
                Ok movedTask ->
                    showNotice
                        ("업무 상태를 " ++ Task.statusLabel movedTask.status ++ " 상태로 변경했습니다.")
                        { model | tasks = List.map (replaceTask movedTask) model.tasks, loading = False }

                Err RequestFailed ->
                    showNotice "상태를 변경하지 못했습니다. 다시 시도해 주세요." { model | loading = False }

        DismissNotice version ->
            if model.noticeVersion == version then
                ( { model | notice = Nothing }, [] )

            else
                ( model, [] )


updateForm : (TaskInput -> TaskInput) -> Model -> Model
updateForm transform model =
    { model | draft = transform model.draft }


toInput : Task -> TaskInput
toInput task =
    { title = task.title, description = task.description, status = task.status, urgency = task.urgency, importance = task.importance, taskOwner = task.taskOwner, outcomeOwner = task.outcomeOwner, expectedResult = task.expectedResult }


replaceTask : Task -> Task -> Task
replaceTask movedTask currentTask =
    if currentTask.taskId == movedTask.taskId then
        movedTask

    else
        currentTask


showNotice : String -> Model -> ( Model, List Effect )
showNotice message model =
    let
        nextVersion =
            model.noticeVersion + 1
    in
    ( { model | notice = Just message, noticeVersion = nextVersion }, [ ClearNoticeAfter nextVersion ] )


addEffect : Effect -> ( Model, List Effect ) -> ( Model, List Effect )
addEffect effect ( model, effects ) =
    ( model, effect :: effects )
