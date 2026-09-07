module ActivityTest exposing (tests)

import Api.Decode exposing (auditDecoder)
import App.Update as Main
import AppFixture exposing (mapPage, mapSession)
import Domain exposing (..)
import Expect
import Html.Attributes as Attr
import Json.Decode as D
import ListViewTest exposing (ready, sample, step)
import Page exposing (Page(..))
import Page.Activity
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)
import Ui.Activity as Activity
import Ui.ListView exposing (Mode(..))


event : Audit
event =
    { seq = 4, at = "2026-09-07T23:59:59.123456Z", actor = Just "p", description = "raw legacy g p", evaluatedGoal = Nothing, evaluatedStatus = Nothing, activity = { tag = "OwnerAssigned", targetKind = "goal", targetId = "g", personId = Just "p", detail = "", reviewId = Nothing, raw = "{\"tag\":\"OwnerAssigned\"}" } }


workspace : Workspace
workspace =
    { sample | events = [ { event | seq = 5, at = "2026-09-08T00:00:00Z" }, event, { event | seq = 3, at = "2026-09-07T00:00:00Z" }, { event | seq = 2, at = "2026-09-06T23:59:59Z" } ] }


tests : Test
tests =
    describe "활동 기록"
        [ test "현재 이름으로 책임 관계를 설명한다" <| \_ -> Activity.description workspace event |> Expect.equal "매출 개선의 책임자를 김직원으로 지정"
        , test "삭제되거나 누락된 대상은 ID를 보존한다" <| \_ -> Activity.description { workspace | people = [], goals = [] } event |> Expect.equal "g의 책임자를 p으로 지정"
        , test "UTC 표시에서 소수 초를 줄이고 시간대를 명시한다" <| \_ -> Activity.timestamp event.at |> Expect.equal "2026-09-07 23:59:59 UTC"
        , test "UTC 하루의 처음과 마지막은 포함하고 다음 날은 제외한다" <| \_ -> Activity.filtered { query = "", kind = "책임", from = "2026-09-07", until = "2026-09-07", review = Nothing } workspace |> List.map .seq |> Expect.equal [ 4, 3 ]
        , test "검색과 유형 및 기간은 함께 적용된다" <| \_ -> Activity.filtered { query = " 김직원 ", kind = "책임", from = "2026-09-07", until = "2026-09-07", review = Nothing } workspace |> List.map .seq |> Expect.equal [ 4, 3 ]
        , test "유형 불일치는 검색어가 일치해도 제외한다" <| \_ -> Activity.filtered { query = "김직원", kind = "권한", from = "", until = "", review = Nothing } workspace |> Expect.equal []
        , test "알 수 없는 이벤트 설명은 원문을 유지한다" <| \_ -> Activity.description workspace { event | activity = { tag = "Future", targetKind = "", targetId = "", personId = Nothing, detail = "", reviewId = Nothing, raw = "{}" } } |> Expect.equal event.description
        , test "기록 주체는 미인증임을 표시한다" <| \_ -> Expect.equal ( "김직원 (미인증)", "로컬 운영자 (미인증)" ) ( Activity.actorName workspace event, Activity.actorName workspace { event | actor = Nothing } )
        , test "회고 필터는 같은 목표의 다른 회고를 제외한다" <|
            \_ ->
                let
                    facts =
                        event.activity

                    reviews =
                        { workspace | events = [ { event | activity = { facts | reviewId = Just "r-a" } }, { event | seq = 9, activity = { facts | reviewId = Just "r-b" } } ] }
                in
                Activity.filtered { query = "", kind = "", from = "", until = "", review = Just "r-a" } reviews |> List.map .seq |> Expect.equal [ 4 ]
        , test "회고에서 활동 기록을 열면 이전 검색 필터를 초기화한다" <|
            \_ ->
                ready |> step (Main.ActivityChange { query = "none", kind = "권한", from = "2030-01-01", until = "2030-01-02", review = Nothing }) |> step (Main.OpenReviewActivity "r") |> (\m -> ( m.pageState.page, m.pageState.activity )) |> Expect.equal ( ActivityLog, { query = "", kind = "", from = "", until = "", review = Just "r" } )
        , test "조직 변경은 활동 필터를 격리한다" <|
            \_ ->
                ready |> step (Main.ActivityChange { query = "secret", kind = "책임", from = "", until = "", review = Just "r" }) |> step (Main.Navigate Dashboard (Just "org-b")) |> .pageState |> .activity |> Expect.equal Activity.init
        , test "활동 표는 원본과 기록 순번을 확인할 상세를 제공한다" <| \_ -> Page.Activity.view Table Activity.init (always ()) workspace |> Query.fromHtml |> Query.has [ tag "details", text event.activity.raw ]
        , test "가이드 전체 활동 기록 링크는 회고와 검색 필터를 초기화한다" <|
            \_ ->
                ready |> step (Main.OpenReviewActivity "r") |> step (Main.Guide ActivityLog "audit-history") |> .pageState |> .activity |> Expect.equal Activity.init
        ]
