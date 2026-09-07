module Page exposing (Page(..), pageName)


type Page
    = Organizations
    | Discovery
    | Workflows
    | AgentDrafts
    | People
    | Dashboard
    | Responsibility
    | Authorities
    | Results
    | Reviews
    | ActivityLog
    | Settings


pageName : Page -> String
pageName page =
    case page of
        Organizations ->
            "조직 목록"

        Discovery ->
            "조직 현황"

        Workflows ->
            "업무 흐름"

        AgentDrafts ->
            "에이전트 초안"

        People ->
            "구성원"

        Dashboard ->
            "목표"

        Responsibility ->
            "책임"

        Authorities ->
            "권한"

        Results ->
            "결과"

        Reviews ->
            "학습"

        ActivityLog ->
            "활동 기록"

        Settings ->
            "조직 설정"
