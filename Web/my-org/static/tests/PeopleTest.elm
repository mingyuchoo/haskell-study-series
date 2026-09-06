module PeopleTest exposing (tests)

import Domain exposing (Person)
import Expect
import Page.People exposing (matches)
import Test exposing (..)


person : Person
person =
    { id = "p", name = "김직원", role = "Engineer", reportsTo = Nothing, department = Just "Platform", email = Just "person@example.com", active = True }


tests : Test
tests =
    describe "구성원 검색과 상태 필터"
        [ test "이름 역할 부서 이메일을 공백 제거와 대소문자 무시로 검색한다" <|
            \_ ->
                [ " 김직원 ", "ENGINEER", "platform", "EXAMPLE.COM" ]
                    |> List.map (\query -> matches query "active" person)
                    |> Expect.equal [ True, True, True, True ]
        , test "비활성 구성원은 재직 필터에서 제외하고 전체와 비활성에서 조회한다" <|
            \_ ->
                [ "active", "inactive", "all" ]
                    |> List.map (\status -> matches "" status { person | active = False })
                    |> Expect.equal [ False, True, True ]
        , test "검색어 불일치와 비어 있는 선택 프로필을 처리한다" <|
            \_ ->
                matches "missing" "all" { person | department = Nothing, email = Nothing }
                    |> Expect.equal False
        ]
