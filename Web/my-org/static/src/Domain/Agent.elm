module Domain.Agent exposing (Approval(..), Change(..), Role, Snapshot, apply, approvalKey, levelLabel, levels, parseApproval, problems, toolsText)

import Domain exposing (Diagnostic)


{-| 사람 승인 주체. 구성원 한 명이거나 특정 결정 권한을 가진 사람이다.
-}
type Approval
    = Person String
    | Permission String


{-| 에이전트 역할. 설계 기록일 뿐 실행 권한이나 도구 접근을 부여하지 않는다.
-}
type alias Role =
    { id : String
    , name : String
    , sourceWorkflow : Maybe String
    , task : String
    , inputs : String
    , outputs : String
    , tools : List String
    , level : String
    , approval : Maybe Approval
    , handoffTo : List String
    , status : String
    , evidence : String
    }


{-| 서버 응답: 저장된 설계, 현재 업무 흐름에서 도출한 초안, 각각의 진단.
-}
type alias Snapshot =
    { version : Int, agents : List Role, drafts : List Role, diagnostics : List Diagnostic, draftDiagnostics : List Diagnostic }


type Change
    = Import (List Role)
    | Name String String
    | Inputs String String
    | Outputs String String
    | Tools String String
    | Level String String
    | SetApproval String String
    | Handoff String String Bool
    | Status String String
    | Evidence String String
    | Remove String


levels : List ( String, String )
levels =
    [ ( "L0", "L0 읽기" ), ( "L1", "L1 작업 공간 쓰기" ), ( "L2", "L2 외부 영향 · 사람 승인 필요" ), ( "L3", "L3 금지 · 사람이 직접 수행" ) ]


levelLabel : String -> String
levelLabel level =
    levels |> List.filter (Tuple.first >> (==) level) |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault level


approvalKey : Maybe Approval -> String
approvalKey approval =
    case approval of
        Just (Person uid) ->
            "person:" ++ uid

        Just (Permission permission) ->
            "permission:" ++ permission

        Nothing ->
            ""


parseApproval : String -> Maybe Approval
parseApproval key =
    if String.startsWith "person:" key then
        Just (Person (String.dropLeft 7 key))

    else if String.startsWith "permission:" key then
        Just (Permission (String.dropLeft 11 key))

    else
        Nothing


toolsText : List String -> String
toolsText =
    String.join ", "


splitTools : String -> List String
splitTools =
    String.split "," >> List.concatMap (String.split "\n") >> List.map String.trim >> List.filter (not << String.isEmpty)


mapRole : String -> (Role -> Role) -> List Role -> List Role
mapRole ident f =
    List.map
        (\role ->
            if role.id == ident then
                f role

            else
                role
        )


apply : Change -> List Role -> List Role
apply change roles =
    case change of
        Import drafts ->
            drafts

        Name ident value ->
            mapRole ident (\r -> { r | name = value }) roles

        Inputs ident value ->
            mapRole ident (\r -> { r | inputs = value }) roles

        Outputs ident value ->
            mapRole ident (\r -> { r | outputs = value }) roles

        Tools ident value ->
            mapRole ident (\r -> { r | tools = splitTools value }) roles

        Level ident value ->
            mapRole ident (\r -> { r | level = value }) roles

        SetApproval ident value ->
            mapRole ident (\r -> { r | approval = parseApproval value }) roles

        Handoff ident target selected ->
            mapRole ident
                (\r ->
                    let
                        without =
                            List.filter ((/=) target) r.handoffTo
                    in
                    { r
                        | handoffTo =
                            if selected && target /= ident then
                                without ++ [ target ]

                            else
                                without
                    }
                )
                roles

        Status ident value ->
            mapRole ident (\r -> { r | status = value }) roles

        Evidence ident value ->
            mapRole ident (\r -> { r | evidence = value }) roles

        Remove ident ->
            roles
                |> List.filter (\r -> r.id /= ident)
                |> List.map (\r -> { r | handoffTo = List.filter ((/=) ident) r.handoffTo })


problems : List Role -> List String
problems roles =
    List.concatMap
        (\role ->
            (if String.trim role.name == "" then
                [ role.id ++ ": 역할 이름을 입력하세요." ]

             else
                []
            )
                ++ (if role.status == "confirmed" && String.trim role.evidence == "" then
                        [ role.name ++ ": 확인된 사실에는 근거가 필요합니다." ]

                    else
                        []
                   )
        )
        roles
