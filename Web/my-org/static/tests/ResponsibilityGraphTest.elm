module ResponsibilityGraphTest exposing (tests)

import App.Update as Main
import AppFixture exposing (mapPage, mapSession)
import Domain exposing (..)
import Expect
import Form.Action exposing (Action(..))
import GraphFixture exposing (..)
import Html.Attributes as Attr
import Json.Encode as Encode
import Page exposing (Page(..))
import Remote exposing (Remote(..))
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)
import Ui.ListView as ListView
import Ui.ResponsibilityGraph as Graph


type Msg
    = GraphMessage Graph.Msg
    | Go String


render state w =
    Graph.view state (Just GraphMessage) (Just Go) w |> Query.fromHtml


selected key =
    Graph.update (Graph.Select key) Graph.init


tests : Test
tests =
    describe "책임 관계 다이어그램"
        [ test "새 검색은 이전 노드 선택을 해제하여 강조 충돌을 방지한다" <|
            \_ -> selected "GoalNode:g" |> Graph.update (Graph.Search "같은 이름") |> .selected |> Expect.equal Nothing
        , test "자원을 숨기면 선택된 자원 상세도 해제한다" <|
            \_ -> Graph.init |> Graph.update (Graph.Resources True) |> Graph.update (Graph.Select "ResourceNode:Budget") |> Graph.update (Graph.Resources False) |> .selected |> Expect.equal Nothing
        , test "Space 노드 활성화는 페이지 스크롤을 막는다" <|
            \_ -> render Graph.init sample |> Query.find [ tag "g", attribute (Attr.attribute "aria-label" "고객 성장 · 권한 부족") ] |> Event.simulate (Event.custom "keydown" (Encode.object [ ( "key", Encode.string " " ) ])) |> Event.expectPreventDefault
        , test "그래프 보기와 메뉴별 카드표 및 책임자 초안은 서로 독립적이다" <|
            \_ ->
                let
                    initial =
                        Main.init { seed = "graph", today = "2026-01-01", deadline = "2026-12-31" } |> Tuple.first

                    ready =
                        initial |> mapSession (\s -> { s | org = Just "org-a", workspace = Loaded sample, fresh = True, syncing = False })

                    step msg m =
                        Main.update msg m |> Tuple.first

                    drafted =
                        ready |> step (Main.Edit (Assign "g") "owner" "p2") |> step (Main.SetListMode Responsibility ListView.Table)

                    graphChanged =
                        drafted |> step (Main.GraphMsg (Graph.Select "GoalNode:g")) |> step (Main.GraphMsg (Graph.Search "고객"))
                in
                mapPage (\p -> { p | graph = drafted.pageState.graph }) graphChanged |> Expect.equal drafted
        , test "같은 조직 메뉴 왕복은 그래프 상태를 보존하고 조직 변경은 초기화한다" <|
            \_ ->
                let
                    initial =
                        Main.init { seed = "graph", today = "2026-01-01", deadline = "2026-12-31" } |> Tuple.first

                    ready =
                        initial |> mapSession (\s -> { s | org = Just "org-a", workspace = Loaded sample, fresh = True, syncing = False })

                    step msg m =
                        Main.update msg m |> Tuple.first

                    changed =
                        ready |> step (Main.GraphMsg (Graph.Select "GoalNode:g")) |> step (Main.GraphMsg (Graph.Search "고객"))

                    returned =
                        changed |> step (Main.Navigate Dashboard (Just "org-a")) |> step (Main.Navigate Responsibility (Just "org-a"))

                    other =
                        returned |> step (Main.Navigate Responsibility (Just "org-b"))
                in
                Expect.equal ( changed.pageState.graph, Graph.init ) ( returned.pageState.graph, other.pageState.graph )
        , test "같은 노드는 중복 연결에도 한 번만 생성하고 같은 이름의 서로 다른 ID는 보존한다" <|
            \_ -> Graph.nodes sample |> List.map Graph.nodeKey |> Expect.equal [ "GoalNode:g", "GoalNode:isolated", "MetricNode:m", "PersonNode:p", "PersonNode:p2", "ResourceNode:Budget", "ResourceNode:Pricing" ]
        , test "연결이 전혀 없는 목표도 노드로 포함한다" <|
            \_ -> Graph.nodes { workspace | goals = [ goal ] } |> Expect.equal [ node "GoalNode" "g" ]
        , test "실제 지표 이름과 한국어 자원 이름을 표시한다" <|
            \_ -> List.map (Graph.nodeLabel sample) [ node "MetricNode" "m", node "ResourceNode" "Budget", node "ResourceNode" "Pricing" ] |> Expect.equal [ "신규 고객 수", "예산", "가격 결정" ]
        , test "하위 목표 방향은 변경하지 않고 한글 관계명을 제공한다" <|
            \_ -> List.map Graph.relationLabel [ "Owns", "DependsOn", "Measures", "Controls" ] |> Expect.equal [ "책임", "하위 목표", "측정 지표", "보유 권한" ]
        , test "기본 화면은 책임과 측정 지표 관계만 표시한다" <|
            \_ -> Graph.visibleEdges Graph.init sample.edges |> List.map .kind |> Expect.equal [ "Owns", "Owns", "Measures" ]
        , test "하위 목표와 자원 토글은 독립적이다" <|
            \_ -> Graph.visibleEdges (Graph.update (Graph.Dependencies True) Graph.init) sample.edges |> List.map .kind |> Expect.equal [ "Owns", "Owns", "Measures", "DependsOn" ]
        , test "자원 토글은 권한 및 예산 연결을 포함한다" <|
            \_ -> Graph.visibleEdges (Graph.update (Graph.Resources True) Graph.init) sample.edges |> List.filter (.kind >> (==) "Controls") |> List.length |> Expect.equal 2
        , test "확대에는 상한과 하한이 있다" <|
            \_ -> ( (Graph.update (Graph.Zoom 100) Graph.init).zoom, (Graph.update (Graph.Zoom -100) Graph.init).zoom ) |> Expect.equal ( 3, 1 )
        , test "전체 맞춤은 검색 및 선택을 유지한 채 확대율을 초기화한다" <|
            \_ ->
                let
                    state =
                        selected "GoalNode:g" |> Graph.update (Graph.Search "고객")
                in
                state |> Graph.update (Graph.Zoom 1) |> Graph.update Graph.Fit |> Expect.equal state
        , test "보기 전환은 선택 검색 및 토글을 보존한다" <|
            \_ ->
                let
                    state =
                        selected "PersonNode:p" |> Graph.update (Graph.Search "고객") |> Graph.update (Graph.Resources True)
                in
                state |> Graph.update (Graph.SetDiagram False) |> Graph.update (Graph.SetDiagram True) |> Expect.equal state
        , test "목표 노드는 미지정과 권한 부족을 구분한다" <|
            \_ -> render Graph.init sample |> Query.has [ attribute (Attr.attribute "aria-label" "고객 성장 · 권한 부족") ]
        , test "미지정 목표에는 권한 부족 대신 책임자 미지정을 표시한다" <|
            \_ -> render Graph.init sample |> Query.has [ attribute (Attr.attribute "aria-label" "연결 없는 목표 · 책임자 미지정") ]
        , test "노드 클릭으로 상세 선택 메시지를 전달한다" <|
            \_ -> render Graph.init sample |> Query.find [ tag "g", attribute (Attr.attribute "aria-label" "고객 성장 · 권한 부족") ] |> Event.simulate Event.click |> Event.expect (GraphMessage (Graph.Select "GoalNode:g"))
        , test "Enter 키로 노드를 선택할 수 있다" <|
            \_ -> render Graph.init sample |> Query.find [ tag "g", attribute (Attr.attribute "aria-label" "고객 성장 · 권한 부족") ] |> Event.simulate (Event.custom "keydown" (Encode.object [ ( "key", Encode.string "Enter" ) ])) |> Event.expect (GraphMessage (Graph.Select "GoalNode:g"))
        , test "Space 키로 노드를 선택할 수 있다" <|
            \_ -> render Graph.init sample |> Query.find [ tag "g", attribute (Attr.attribute "aria-label" "고객 성장 · 권한 부족") ] |> Event.simulate (Event.custom "keydown" (Encode.object [ ( "key", Encode.string " " ) ])) |> Event.expect (GraphMessage (Graph.Select "GoalNode:g"))
        , test "상세에서 기존 책임자 폼으로 이동한다" <|
            \_ -> render (selected "GoalNode:g") sample |> Query.find [ tag "button", Test.Html.Selector.containing [ text "책임자 지정 폼으로 이동" ] ] |> Event.simulate Event.click |> Event.expect (Go "owner-g")
        , test "검색은 다이어그램 노드를 제거하지 않아 연결 맥락을 유지한다" <|
            \_ -> render (Graph.update (Graph.Search "없는 검색어") Graph.init) sample |> Query.findAll [ tag "g", attribute (Attr.attribute "role" "button") ] |> Query.count (Expect.equal 5)
        , test "검색 일치가 없는 관계 목록은 빈 상태를 안내한다" <|
            \_ -> render (Graph.init |> Graph.update (Graph.Search "없는 검색어") |> Graph.update (Graph.SetDiagram False)) sample |> Query.has [ text "검색 조건에 맞는 관계가 없습니다." ]
        , test "빈 조직도 안내와 보기 도구를 렌더링한다" <|
            \_ -> render Graph.init workspace |> Query.has [ text "아직 구성원과 목표가 없습니다. 구성원이나 목표를 추가하면 관계를 확인할 수 있습니다." ]
        , test "선택 상세에 숨긴 관계와 필요한 예산도 표시한다" <|
            \_ -> render (selected "GoalNode:g") sample |> Query.has [ text "하위 목표", text "100원", text "권한 통제율 50%" ]
        ]
