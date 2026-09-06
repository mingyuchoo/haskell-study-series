module Ui.Activity exposing (State, actorName, category, description, filtered, init, targetName, timestamp)

import Domain exposing (..)
import Ui.Label exposing (goalName, personName)


type alias State =
    { query : String, kind : String, from : String, until : String, review : Maybe String }


init : State
init =
    { query = "", kind = "", from = "", until = "", review = Nothing }


category : Audit -> String
category event =
    case event.activity.tag of
        "OwnerAssigned" ->
            "책임"

        "AuthorityGranted" ->
            "권한"

        "AuthorityRevoked" ->
            "권한"

        "ResultReported" ->
            "결과"

        "GoalEvaluated" ->
            "결과"

        "ReviewHeld" ->
            "학습"

        "StrategyChanged" ->
            "학습"

        _ ->
            case event.activity.targetKind of
                "person" ->
                    "구성원"

                "goal" ->
                    "목표"

                "organization" ->
                    "조직"

                _ ->
                    "기타"


targetName : Workspace -> Audit -> String
targetName w event =
    case event.activity.targetKind of
        "person" ->
            personName w event.activity.targetId

        "goal" ->
            goalName w event.activity.targetId

        "organization" ->
            if event.activity.targetId == w.organization.id then
                w.organization.name

            else
                event.activity.targetId

        _ ->
            "—"


actorName : Workspace -> Audit -> String
actorName w event =
    event.actor |> Maybe.map (\ident -> personName w ident ++ " (미인증)") |> Maybe.withDefault "로컬 운영자 (미인증)"


description : Workspace -> Audit -> String
description w event =
    let
        target =
            targetName w event

        person =
            event.activity.personId |> Maybe.map (personName w) |> Maybe.withDefault ""

        withDetail label =
            target
                ++ " · "
                ++ label
                ++ (if event.activity.detail == "" then
                        ""

                    else
                        " · " ++ event.activity.detail
                   )
    in
    case event.activity.tag of
        "OwnerAssigned" ->
            target ++ "의 책임자를 " ++ person ++ "으로 지정"

        "OrganizationCreated" ->
            withDetail "조직 생성"

        "OrganizationRenamed" ->
            withDetail "조직 이름 변경"

        "OrganizationDeleted" ->
            withDetail "조직 삭제"

        "DemoSeeded" ->
            withDetail "체험 데이터 생성"

        "PersonAdded" ->
            withDetail "구성원 추가"

        "EmployeeAdded" ->
            withDetail "구성원 추가"

        "PersonUpdated" ->
            withDetail "구성원 정보 수정"

        "PersonDeactivated" ->
            withDetail "구성원 비활성화"
                ++ (if person == "" then
                        ""

                    else
                        " · 후임 " ++ person
                   )

        "GoalCreated" ->
            withDetail "목표 생성"

        "GoalActivated" ->
            withDetail "목표 활성화"

        "AuthorityGranted" ->
            withDetail "권한 부여"

        "AuthorityRevoked" ->
            withDetail "권한 회수"

        "ResultReported" ->
            withDetail "결과 보고"
                ++ (if person == "" then
                        ""

                    else
                        " · 보고자 " ++ person
                   )

        "GoalEvaluated" ->
            withDetail "목표 평가"

        "ReviewHeld" ->
            withDetail "회고 기록"

        "StrategyChanged" ->
            withDetail "전략 변경"

        _ ->
            event.description


timestamp : String -> String
timestamp value =
    if String.contains "T" value && String.endsWith "Z" value then
        String.left 10 value ++ " " ++ String.slice 11 19 value ++ " UTC"

    else
        value


filtered : State -> Workspace -> List Audit
filtered state w =
    w.events
        |> List.filter
            (\event ->
                (state.kind == "" || category event == state.kind)
                    && (state.from == "" || String.left 10 event.at >= state.from)
                    && (state.until == "" || String.left 10 event.at <= state.until)
                    && (state.review == Nothing || event.activity.reviewId == state.review)
                    && (String.toLower (String.join " " [ targetName w event, actorName w event, description w event, event.activity.targetId, event.activity.personId |> Maybe.withDefault "", event.activity.reviewId |> Maybe.withDefault "", event.actor |> Maybe.withDefault "", category event ]) |> String.contains (String.toLower (String.trim state.query)))
            )
        |> List.sortBy (.seq >> negate)
