module Domain.Task exposing
    ( Importance(..)
    , InputError(..)
    , Quadrant(..)
    , Status(..)
    , Task
    , TaskInput
    , Urgency(..)
    , allStatuses
    , canReviewResult
    , canSubmitResult
    , emptyInput
    , importanceFromString
    , importanceLabel
    , importanceString
    , quadrantClass
    , quadrantLabel
    , quadrantOf
    , resultStateLabel
    , statusClass
    , statusFromString
    , statusLabel
    , statusString
    , urgencyFromString
    , urgencyLabel
    , urgencyString
    , validateInput
    )


type Status
    = Draft
    | Reviewed
    | Submitted
    | Approved
    | Effective


type Urgency
    = Urgent
    | NotUrgent


type Importance
    = Important
    | NotImportant


type Quadrant
    = DoFirst
    | Schedule
    | Delegate
    | Eliminate


type alias Task =
    { taskId : Int
    , title : String
    , description : String
    , status : Status
    , urgency : Urgency
    , importance : Importance
    , taskOwner : String
    , outcomeOwner : String
    , expectedResult : String
    , submittedResult : Maybe String
    , reviewComment : Maybe String
    }


type alias TaskInput =
    { title : String
    , description : String
    , status : Status
    , urgency : Urgency
    , importance : Importance
    , taskOwner : String
    , outcomeOwner : String
    , expectedResult : String
    }


type InputError
    = TitleRequired


emptyInput : TaskInput
emptyInput =
    { title = "", description = "", status = Draft, urgency = NotUrgent, importance = Important, taskOwner = "", outcomeOwner = "", expectedResult = "" }


validateInput : TaskInput -> Result InputError TaskInput
validateInput input =
    if String.trim input.title == "" then
        Err TitleRequired

    else
        Ok input


allStatuses : List Status
allStatuses =
    [ Draft, Reviewed, Submitted, Approved, Effective ]


statusString : Status -> String
statusString taskStatus =
    case taskStatus of
        Draft ->
            "Draft"

        Reviewed ->
            "Reviewed"

        Submitted ->
            "Submitted"

        Approved ->
            "Approved"

        Effective ->
            "Effective"


statusFromString : String -> Maybe Status
statusFromString rawStatus =
    case rawStatus of
        "Draft" ->
            Just Draft

        "Reviewed" ->
            Just Reviewed

        "Submitted" ->
            Just Submitted

        "Approved" ->
            Just Approved

        "Effective" ->
            Just Effective

        _ ->
            Nothing


statusLabel : Status -> String
statusLabel taskStatus =
    case taskStatus of
        Draft ->
            "초안"

        Reviewed ->
            "검토 완료"

        Submitted ->
            "제출됨"

        Approved ->
            "승인됨"

        Effective ->
            "효력 발생"


statusClass : Status -> String
statusClass taskStatus =
    "status-" ++ String.toLower (statusString taskStatus)


urgencyString : Urgency -> String
urgencyString taskUrgency =
    case taskUrgency of
        Urgent ->
            "Urgent"

        NotUrgent ->
            "NotUrgent"


urgencyFromString : String -> Maybe Urgency
urgencyFromString rawUrgency =
    case rawUrgency of
        "Urgent" ->
            Just Urgent

        "NotUrgent" ->
            Just NotUrgent

        _ ->
            Nothing


urgencyLabel : Urgency -> String
urgencyLabel taskUrgency =
    case taskUrgency of
        Urgent ->
            "긴급"

        NotUrgent ->
            "긴급하지 않음"


importanceString : Importance -> String
importanceString taskImportance =
    case taskImportance of
        Important ->
            "Important"

        NotImportant ->
            "NotImportant"


importanceFromString : String -> Maybe Importance
importanceFromString rawImportance =
    case rawImportance of
        "Important" ->
            Just Important

        "NotImportant" ->
            Just NotImportant

        _ ->
            Nothing


importanceLabel : Importance -> String
importanceLabel taskImportance =
    case taskImportance of
        Important ->
            "중요"

        NotImportant ->
            "중요하지 않음"


quadrantOf : Urgency -> Importance -> Quadrant
quadrantOf taskUrgency taskImportance =
    case ( taskUrgency, taskImportance ) of
        ( Urgent, Important ) ->
            DoFirst

        ( NotUrgent, Important ) ->
            Schedule

        ( Urgent, NotImportant ) ->
            Delegate

        ( NotUrgent, NotImportant ) ->
            Eliminate


quadrantLabel : Quadrant -> String
quadrantLabel quadrant =
    case quadrant of
        DoFirst ->
            "즉시 실행"

        Schedule ->
            "계획 수립"

        Delegate ->
            "위임"

        Eliminate ->
            "제거"


quadrantClass : Quadrant -> String
quadrantClass quadrant =
    case quadrant of
        DoFirst ->
            "quadrant-do-first"

        Schedule ->
            "quadrant-schedule"

        Delegate ->
            "quadrant-delegate"

        Eliminate ->
            "quadrant-eliminate"


{-| Task Owner가 결과물을 제출(또는 재제출)할 수 있는 상태인지 판단한다.
-}
canSubmitResult : Task -> Bool
canSubmitResult task =
    case task.status of
        Draft ->
            True

        Reviewed ->
            True

        _ ->
            False


{-| Outcome Owner가 제출된 결과물을 승인하거나 수정 요청할 수 있는지 판단한다.
-}
canReviewResult : Task -> Bool
canReviewResult task =
    task.status == Submitted && task.submittedResult /= Nothing


{-| 카드에 짧게 표시할 결과물 진행 상태 레이블.
-}
resultStateLabel : Task -> String
resultStateLabel task =
    case ( task.status, task.submittedResult, task.reviewComment ) of
        ( Effective, _, _ ) ->
            "효력 발생"

        ( Approved, _, _ ) ->
            "승인 완료"

        ( Submitted, Nothing, _ ) ->
            "결과물 없음"

        ( Submitted, Just _, _ ) ->
            "리뷰 대기"

        ( _, Nothing, _ ) ->
            "결과물 미제출"

        ( Reviewed, Just _, Just _ ) ->
            "수정 요청됨"

        ( _, Just _, _ ) ->
            "재제출 가능"
