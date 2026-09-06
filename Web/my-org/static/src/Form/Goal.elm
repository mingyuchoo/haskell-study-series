module Form.Goal exposing (Draft, Field(..), Validated, edit, fieldName, fromKey, fromValues, validate, value)

import Domain.Permission exposing (permissionKeys)


type Field
    = Description
    | MetricName
    | Unit
    | MetricId
    | Direction
    | Baseline
    | Target
    | StartsAt
    | Deadline
    | Budget
    | Parent
    | Permission String


fieldName : Field -> String
fieldName field =
    case field of
        Description ->
            "description"

        MetricName ->
            "metricName"

        Unit ->
            "unit"

        MetricId ->
            "metricId"

        Direction ->
            "direction"

        Baseline ->
            "baseline"

        Target ->
            "target"

        StartsAt ->
            "startsAt"

        Deadline ->
            "deadline"

        Budget ->
            "budget"

        Parent ->
            "parent"

        Permission key ->
            key


fromKey : String -> Maybe Field
fromKey key =
    case key of
        "description" ->
            Just Description

        "metricName" ->
            Just MetricName

        "unit" ->
            Just Unit

        "metricId" ->
            Just MetricId

        "direction" ->
            Just Direction

        "baseline" ->
            Just Baseline

        "target" ->
            Just Target

        "startsAt" ->
            Just StartsAt

        "deadline" ->
            Just Deadline

        "budget" ->
            Just Budget

        "parent" ->
            Just Parent

        _ ->
            if List.member key permissionKeys then
                Just (Permission key)

            else
                Nothing


type alias Draft =
    { description : String, metricName : String, unit : String, metricId : String, direction : String, baseline : String, target : String, startsAt : String, deadline : String, budget : String, parent : String, permissions : List String }


type alias Validated =
    { description : String, metricName : String, unit : String, metricId : String, direction : String, baseline : Float, target : Float, startsAt : String, deadline : String, budget : Float, parent : String, permissions : List String }


fromValues : (String -> String) -> Draft
fromValues get =
    { description = get "description", metricName = get "metricName", unit = get "unit", metricId = get "metricId", direction = get "direction", baseline = get "baseline", target = get "target", startsAt = get "startsAt", deadline = get "deadline", budget = get "budget", parent = get "parent", permissions = permissionKeys |> List.filter (\key -> get key == "true") }


value : Draft -> Field -> String
value draft key =
    case key of
        Description ->
            draft.description

        MetricName ->
            draft.metricName

        Unit ->
            draft.unit

        MetricId ->
            draft.metricId

        Direction ->
            draft.direction

        Baseline ->
            draft.baseline

        Target ->
            draft.target

        StartsAt ->
            draft.startsAt

        Deadline ->
            draft.deadline

        Budget ->
            draft.budget

        Parent ->
            draft.parent

        Permission permission ->
            if List.member permission draft.permissions then
                "true"

            else
                "false"


edit : Field -> String -> Draft -> Draft
edit key content draft =
    case key of
        Description ->
            { draft | description = content }

        MetricName ->
            { draft | metricName = content }

        Unit ->
            { draft | unit = content }

        MetricId ->
            { draft | metricId = content }

        Direction ->
            { draft | direction = content }

        Baseline ->
            { draft | baseline = content }

        Target ->
            { draft | target = content }

        StartsAt ->
            { draft | startsAt = content }

        Deadline ->
            { draft | deadline = content }

        Budget ->
            { draft | budget = content }

        Parent ->
            { draft | parent = content }

        Permission permission ->
            { draft
                | permissions =
                    if content == "true" then
                        permission :: List.filter ((/=) permission) draft.permissions

                    else
                        List.filter ((/=) permission) draft.permissions
            }


validate : Draft -> Result String Validated
validate draft =
    if List.any (String.trim >> (==) "") [ draft.description, draft.metricId, draft.metricName, draft.unit, draft.startsAt, draft.deadline ] then
        Err "필수 항목을 모두 입력하세요."

    else
        case ( String.toFloat draft.baseline, String.toFloat draft.target, String.toFloat draft.budget ) of
            ( Just baseline, Just target, Just budget ) ->
                if draft.deadline < draft.startsAt then
                    Err "마감일은 시작일 이후여야 합니다."

                else
                    Ok { description = draft.description, metricName = draft.metricName, unit = draft.unit, metricId = draft.metricId, direction = draft.direction, baseline = baseline, target = target, startsAt = draft.startsAt, deadline = draft.deadline, budget = budget, parent = draft.parent, permissions = draft.permissions }

            _ ->
                Err "숫자 항목을 올바르게 입력하세요."
