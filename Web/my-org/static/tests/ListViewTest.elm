module ListViewTest exposing (ready, sample, step, tests)

import Dict
import Domain exposing (..)
import Expect
import Form.Action exposing (Action(..))
import Form.Goal as Goal
import Form.Review as Review
import Html exposing (Html)
import Html.Attributes as Attr
import Main
import Page exposing (Page(..), pageName)
import Page.Activity
import Page.Authorities
import Page.Goals
import Page.Learning
import Page.Organizations
import Page.People
import Page.Responsibility
import Page.Results
import Remote exposing (Remote(..))
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)
import Ui.Activity
import Ui.ListView exposing (Mode(..))


type Msg
    = Submit Action
    | Edit Action String String
    | Open String
    | SettingsFor String
    | Change Mode
    | Other


forms =
    { busy = False, fresh = True, saving = Nothing, value = \_ _ -> "", edit = Edit, submit = Submit }


workspace : Workspace
workspace =
    { organization = { id = "org-a", name = "Alpha", createdAt = "2026-01-01" }, version = 1, demo = False, people = [], goals = [], authorities = [], reviews = [], compiler = { errors = 0, warnings = 0, diagnostics = [] }, edges = [], events = [], decisionShare = Dict.empty, reviewWarnings = [] }


ready : Main.Model
ready =
    let
        initial =
            Main.init { seed = "test", today = "2026-01-01", deadline = "2026-12-31" } |> Tuple.first
    in
    { initial | org = Just "org-a", workspace = Loaded workspace, fresh = True, syncing = False }


step : Main.Msg -> Main.Model -> Main.Model
step msg model =
    Main.update msg model |> Tuple.first


person : Person
person =
    { id = "p", name = "김직원", role = "Engineer", department = Just "개발", email = Just "p@example.com", reportsTo = Nothing, active = True }


goal : GoalView
goal =
    { goal = { id = "g", description = "매출 개선", metric = { id = "m", name = "매출", unit = "원", direction = "HigherIsBetter" }, baseline = 0, target = 100, deadline = "2026-12-31", requiredBudget = 0, requiredPermissions = [] }, owner = Just "p", active = False, evaluation = { status = NoData, progress = 0, latestValue = Nothing }, analysis = { coverage = 1, possibleCause = "측정 필요" }, results = [], strategies = [] }


sample : Workspace
sample =
    { workspace | people = [ person ], goals = [ goal ], reviews = [ { id = "r", goal = "g", heldAt = "2026-09-07", note = "첫 회고", evaluation = goal.evaluation, learnings = [ "고객 의견" ], decisions = [ { text = "개선 진행", owner = "p", deadline = Just "2026-12-31" } ] } ], events = [ { seq = 1, at = "2026-09-07", actor = Just "p", description = "목표 등록", evaluatedGoal = Nothing, evaluatedStatus = Nothing, activity = { tag = "", targetKind = "", targetId = "", personId = Nothing, detail = "", reviewId = Nothing, raw = "null" } } ] }


pageHtml : Mode -> Page -> Workspace -> Html Msg
pageHtml mode page w =
    case page of
        Organizations ->
            Page.Organizations.viewWith mode { forms = forms, organizations = Loaded [ { organization = w.organization, peopleCount = List.length w.people, goalCount = List.length w.goals, demo = False } ], open = Open, settings = SettingsFor }

        People ->
            Page.People.viewWith mode { forms = forms, query = "", status = "active", selected = Nothing, search = always Other, filter = always Other, open = Open, reset = always Other, goals = Other } w

        Dashboard ->
            Page.Goals.viewWith mode { forms = forms, expandedGoal = Nothing, results = Open, draft = Goal.fromValues (always ""), edit = \_ _ -> Other } w

        Responsibility ->
            Page.Responsibility.viewWith mode { forms = forms } w

        Authorities ->
            Page.Authorities.viewWith mode { forms = forms } w

        Results ->
            Page.Results.viewWith mode { forms = forms, goals = Open } w

        Reviews ->
            Page.Learning.viewWith mode { forms = forms, draft = Review.fromValues (always ""), edit = \_ _ -> Other } w

        ActivityLog ->
            Page.Activity.view mode Ui.Activity.init (always Other) w

        Settings ->
            Html.text ""


pages : List Page
pages =
    [ Organizations, People, Dashboard, Responsibility, Authorities, Results, Reviews, ActivityLog ]


clickButton : String -> Msg -> Html Msg -> Expect.Expectation
clickButton label expected html =
    html |> Query.fromHtml |> Query.find [ tag "button", Test.Html.Selector.containing [ text label ] ] |> Event.simulate Event.click |> Event.expect expected


tests : Test
tests =
    describe "카드와 표 보기"
        [ test "보기 버튼은 접근 가능한 선택 상태와 전환 이벤트를 제공한다" <|
            \_ -> Ui.ListView.controls Cards Change |> Query.fromHtml |> Query.find [ tag "button", attribute (Attr.attribute "aria-pressed" "false") ] |> Event.simulate Event.click |> Event.expect (Change Table)
        , test "카드 버튼은 현재 선택됨을 알린다" <|
            \_ -> Ui.ListView.controls Cards Change |> Query.fromHtml |> Query.has [ tag "button", text "카드", attribute (Attr.attribute "aria-pressed" "true") ]
        , test "각 메뉴의 선택 값은 서로 덮어쓰지 않는다" <|
            \_ -> ready |> step (Main.SetListMode People Table) |> step (Main.SetListMode Dashboard Cards) |> step (Main.SetListMode Results Table) |> (\m -> List.map (\page -> Dict.get (pageName page) m.listModes) [ People, Dashboard, Results ]) |> Expect.equal [ Just Table, Just Cards, Just Table ]
        , describe "각 메뉴가 실제 표를 렌더링한다"
            (List.map (\page -> test (pageName page) (\_ -> pageHtml Table page sample |> Query.fromHtml |> Query.findAll [ tag "table" ] |> Query.count (Expect.atLeast 1))) pages)
        , describe "빈 목록에서 빈 상태를 유지한다"
            (List.map
                (\( page, message ) -> test (pageName page) (\_ -> pageHtml Table page workspace |> Query.fromHtml |> Query.has [ text message ]))
                [ ( People, "표시할 구성원이 없습니다" ), ( Dashboard, "어떤 결과를 만들고 싶나요?" ), ( Authorities, "구성원을 먼저 추가하세요" ), ( Results, "아직 측정할 목표가 없습니다" ) ]
            )
        , test "표 컨테이너는 키보드 접근과 열 제목을 제공한다" <|
            \_ -> Ui.ListView.tableView "테스트" [ "이름" ] [] |> Query.fromHtml |> Query.has [ attribute (Attr.tabindex 0), attribute (Attr.attribute "role" "region") ]
        , test "기존 책임 view는 표를 기본으로 유지한다" <|
            \_ -> Page.Responsibility.view { forms = forms } sample |> Query.fromHtml |> Query.findAll [ tag "table" ] |> Query.count (Expect.atLeast 1)
        , test "기존 조직 view는 표를 기본으로 제공한다" <|
            \_ -> Page.Organizations.view { forms = forms, organizations = Loaded [ { organization = sample.organization, peopleCount = 1, goalCount = 1, demo = False } ], open = Open, settings = SettingsFor } |> Query.fromHtml |> Query.findAll [ tag "table" ] |> Query.count (Expect.equal 1)
        , test "보기 전환은 입력 초안과 검색 선택 및 서버 상태를 보존한다" <|
            \_ ->
                let
                    drafted =
                        ready |> step (Main.Edit AddPerson "name" "작성 중") |> step (Main.EditGoal Goal.Description "목표 초안") |> step (Main.EditReview Review.Note "회고 초안") |> step (Main.SearchPeople "검색") |> step (Main.FilterPeople "all") |> step (Main.OpenPerson "p")

                    switched =
                        step (Main.SetListMode People Table) drafted
                in
                { switched | listModes = drafted.listModes } |> Expect.equal drafted
        , test "각 메뉴 선택은 독립적이며 왕복 탐색 후 유지된다" <|
            \_ ->
                let
                    selected =
                        ready |> step (Main.SetListMode People Table) |> step (Main.SetListMode Dashboard Cards) |> step (Main.SetListMode Results Table)

                    navigated =
                        selected |> step (Main.Navigate Results (Just "org-a")) |> step (Main.Navigate People (Just "org-a"))
                in
                Expect.all [ \_ -> Expect.equal selected.listModes navigated.listModes, \_ -> Expect.equal 3 (Dict.size selected.listModes) ] ()
        , test "조직 표에서 조직을 열 수 있다" <|
            \_ -> pageHtml Table Organizations sample |> clickButton "조직 열기 →" (Open "org-a")
        , test "조직 표에서 설정으로 이동한다" <|
            \_ -> pageHtml Table Organizations sample |> clickButton "상세 · 수정 · 삭제" (SettingsFor "org-a")
        , test "구성원 표에서 상세 선택을 전달한다" <|
            \_ -> pageHtml Table People sample |> clickButton "상세 · 수정" (Open "p")
        , test "목표 표에서 활성화 액션을 전달한다" <|
            \_ -> pageHtml Table Dashboard sample |> clickButton "목표 활성화" (Submit (Activate "g"))
        , test "목표 표에서 결과로 이동한다" <|
            \_ -> pageHtml Table Dashboard sample |> clickButton "결과 보고 · 평가 →" (Open "goal-g")
        , test "책임 표에서 책임자 입력을 전달한다" <|
            \_ -> pageHtml Table Responsibility sample |> Query.fromHtml |> Query.find [ tag "select", attribute (Attr.name "owner") ] |> Event.simulate (Event.input "p") |> Event.expect (Edit (Assign "g") "owner" "p")
        , test "권한 요약은 편집 초안이 아닌 저장된 예산과 권한을 표시한다" <|
            \_ ->
                let
                    saved =
                        { sample | authorities = [ { owner = "p", budgetLimit = 300, canHire = True, canChangePrice = False, canApprove = [] } ] }

                    editing =
                        { forms
                            | value =
                                \_ key ->
                                    if key == "budget" then
                                        "999"

                                    else
                                        "false"
                        }
                in
                Page.Authorities.viewWith Table { forms = editing } saved |> Query.fromHtml |> Query.findAll [ tag "tbody" ] |> Query.first |> Query.has [ text "300", text "채용" ]
        , test "권한 표에서 예산 입력을 전달한다" <|
            \_ -> pageHtml Table Authorities sample |> Query.fromHtml |> Query.find [ tag "input", attribute (Attr.name "budget") ] |> Event.simulate (Event.input "100") |> Event.expect (Edit (Grant "p") "budget" "100")
        , test "결과 표에서 평가 기록 액션을 전달한다" <|
            \_ -> pageHtml Table Results sample |> clickButton "평가 기록" (Submit (Evaluate "g"))
        , test "결과 표에서 결과 보고 입력을 전달한다" <|
            \_ -> pageHtml Table Results sample |> Query.fromHtml |> Query.find [ tag "input", attribute (Attr.name "value") ] |> Event.simulate (Event.input "42") |> Event.expect (Edit (Report "g") "value" "42")
        , test "학습 표는 학습 결정 담당자를 보존하고 전체 활동 기록은 분리한다" <|
            \_ ->
                Expect.all
                    [ \_ -> pageHtml Table Reviews sample |> Query.fromHtml |> Query.has [ text "고객 의견", text "개선 진행", text "김직원" ]
                    , \_ -> pageHtml Table Reviews sample |> Query.fromHtml |> Query.hasNot [ text "목표 등록" ]
                    ]
                    ()
        ]
