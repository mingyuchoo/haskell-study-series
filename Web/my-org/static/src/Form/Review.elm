module Form.Review exposing (Draft, Field(..), Validated, edit, fieldName, fromKey, fromValues, validate, value)


type Field
    = Goal
    | Note
    | Learning
    | Decision
    | DecisionOwner
    | DecisionDeadline


fieldName : Field -> String
fieldName field =
    case field of
        Goal ->
            "goal"

        Note ->
            "note"

        Learning ->
            "learning"

        Decision ->
            "decision"

        DecisionOwner ->
            "decisionOwner"

        DecisionDeadline ->
            "decisionDeadline"


fromKey : String -> Maybe Field
fromKey key =
    case key of
        "goal" ->
            Just Goal

        "note" ->
            Just Note

        "learning" ->
            Just Learning

        "decision" ->
            Just Decision

        "decisionOwner" ->
            Just DecisionOwner

        "decisionDeadline" ->
            Just DecisionDeadline

        _ ->
            Nothing


type alias Draft =
    { goal : String, note : String, learning : String, decision : String, decisionOwner : String, decisionDeadline : String }


type alias Validated =
    { goal : String, note : String, learning : String, decision : String, decisionOwner : String, decisionDeadline : String }


fromValues : (String -> String) -> Draft
fromValues get =
    { goal = get "goal", note = get "note", learning = get "learning", decision = get "decision", decisionOwner = get "decisionOwner", decisionDeadline = get "decisionDeadline" }


value : Draft -> Field -> String
value draft key =
    case key of
        Goal ->
            draft.goal

        Note ->
            draft.note

        Learning ->
            draft.learning

        Decision ->
            draft.decision

        DecisionOwner ->
            draft.decisionOwner

        DecisionDeadline ->
            draft.decisionDeadline


edit : Field -> String -> Draft -> Draft
edit key content draft =
    case key of
        Goal ->
            { draft | goal = content }

        Note ->
            { draft | note = content }

        Learning ->
            { draft | learning = content }

        Decision ->
            { draft | decision = content }

        DecisionOwner ->
            { draft | decisionOwner = content }

        DecisionDeadline ->
            { draft | decisionDeadline = content }


validate : Draft -> Result String Validated
validate draft =
    if List.any (String.trim >> (==) "") [ draft.goal, draft.note ] then
        Err "필수 항목을 모두 입력하세요."

    else if String.trim draft.decision /= "" && draft.decisionOwner == "" then
        Err "다음 결정의 담당자를 선택하세요."

    else
        Ok draft
