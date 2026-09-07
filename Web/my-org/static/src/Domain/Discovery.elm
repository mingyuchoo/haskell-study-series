module Domain.Discovery exposing (Change(..), Document, Observation, Review, Snapshot, Workflow, apply, empty, emptyWorkflow, problems, statusLabel)


type alias Observation =
    { id : String, subject : String, detail : String, status : String, evidence : String }


{-| 업무 흐름. 자유 텍스트 필드는 그대로 두고 선택적인 참조 필드가 구성원, 결정 권한, 다른 업무를 가리킨다.
-}
type alias Workflow =
    { id : String
    , name : String
    , role : String
    , rolePerson : Maybe String
    , trigger : String
    , inputs : String
    , tools : String
    , outputs : String
    , handoff : String
    , handoffWorkflows : List String
    , approval : String
    , approvalPerson : Maybe String
    , approvalPermission : Maybe String
    , status : String
    , evidence : String
    }


type alias Review =
    { status : String, note : String }


type alias Document =
    { scope : String, asOf : String, observations : List Observation, workflows : List Workflow, review : Review }


type alias Snapshot =
    { version : Int, discovery : Document }


type Change
    = Scope String
    | AsOf String
    | AddObservation String
    | ObservationField String String String
    | AddWorkflow String
    | WorkflowField String String String
    | WorkflowRolePerson String String
    | WorkflowApprovalPerson String String
    | WorkflowApprovalPermission String String
    | WorkflowHandoff String String Bool
    | RemoveObservation String
    | RemoveWorkflow String
    | ReviewNote String
    | ReviewStatus String


empty : Document
empty =
    { scope = "", asOf = "", observations = [], workflows = [], review = { status = "pending", note = "" } }


emptyWorkflow : String -> Workflow
emptyWorkflow ident =
    { id = ident, name = "", role = "", rolePerson = Nothing, trigger = "", inputs = "", tools = "", outputs = "", handoff = "", handoffWorkflows = [], approval = "", approvalPerson = Nothing, approvalPermission = Nothing, status = "unknown", evidence = "" }


statusLabel : String -> String
statusLabel status =
    case status of
        "confirmed" ->
            "확인된 사실"

        "proposed" ->
            "개선안"

        _ ->
            "미확인"


optional : String -> Maybe String
optional value =
    if String.trim value == "" then
        Nothing

    else
        Just value


mapWorkflow : String -> (Workflow -> Workflow) -> Document -> Document
mapWorkflow ident f doc =
    { doc
        | workflows =
            List.map
                (\w ->
                    if w.id == ident then
                        f w

                    else
                        w
                )
                doc.workflows
    }


apply : Change -> Document -> Document
apply change doc =
    let
        updated =
            case change of
                Scope value ->
                    { doc | scope = value }

                AsOf value ->
                    { doc | asOf = value }

                AddObservation ident ->
                    { doc | observations = doc.observations ++ [ { id = ident, subject = "", detail = "", status = "unknown", evidence = "" } ] }

                ObservationField ident key value ->
                    { doc
                        | observations =
                            List.map
                                (\o ->
                                    if o.id == ident then
                                        editObservation key value o

                                    else
                                        o
                                )
                                doc.observations
                    }

                AddWorkflow ident ->
                    { doc | workflows = doc.workflows ++ [ emptyWorkflow ident ] }

                WorkflowField ident key value ->
                    mapWorkflow ident (editWorkflow key value) doc

                WorkflowRolePerson ident value ->
                    mapWorkflow ident (\w -> { w | rolePerson = optional value }) doc

                WorkflowApprovalPerson ident value ->
                    mapWorkflow ident (\w -> { w | approvalPerson = optional value }) doc

                WorkflowApprovalPermission ident value ->
                    mapWorkflow ident (\w -> { w | approvalPermission = optional value }) doc

                WorkflowHandoff ident target selected ->
                    mapWorkflow ident
                        (\w ->
                            let
                                without =
                                    List.filter ((/=) target) w.handoffWorkflows
                            in
                            { w
                                | handoffWorkflows =
                                    if selected && target /= ident then
                                        without ++ [ target ]

                                    else
                                        without
                            }
                        )
                        doc

                RemoveObservation ident ->
                    { doc | observations = List.filter (\o -> o.id /= ident) doc.observations }

                RemoveWorkflow ident ->
                    { doc
                        | workflows =
                            doc.workflows
                                |> List.filter (\w -> w.id /= ident)
                                |> List.map (\w -> { w | handoffWorkflows = List.filter ((/=) ident) w.handoffWorkflows })
                    }

                ReviewNote value ->
                    { doc | review = { status = doc.review.status, note = value } }

                ReviewStatus value ->
                    { doc | review = { status = value, note = doc.review.note } }
    in
    case change of
        ReviewNote _ ->
            updated

        ReviewStatus _ ->
            updated

        _ ->
            { updated | review = { status = "pending", note = updated.review.note } }


editObservation key value o =
    case key of
        "subject" ->
            { o | subject = value }

        "detail" ->
            { o | detail = value }

        "status" ->
            { o | status = value }

        "evidence" ->
            { o | evidence = value }

        _ ->
            o


editWorkflow key value w =
    case key of
        "name" ->
            { w | name = value }

        "role" ->
            { w | role = value }

        "trigger" ->
            { w | trigger = value }

        "inputs" ->
            { w | inputs = value }

        "tools" ->
            { w | tools = value }

        "outputs" ->
            { w | outputs = value }

        "handoff" ->
            { w | handoff = value }

        "approval" ->
            { w | approval = value }

        "status" ->
            { w | status = value }

        "evidence" ->
            { w | evidence = value }

        _ ->
            w


problems : Document -> List String
problems doc =
    let
        blank =
            String.trim >> String.isEmpty

        observation o =
            (if blank o.subject then
                [ "현황 항목의 제목을 입력하세요." ]

             else
                []
            )
                ++ (if o.status == "confirmed" && blank o.evidence then
                        [ o.subject ++ ": 확인된 사실에는 근거가 필요합니다." ]

                    else
                        []
                   )

        workflow w =
            (if blank w.name then
                [ "업무 이름을 입력하세요." ]

             else
                []
            )
                ++ (if w.status == "confirmed" && blank w.evidence then
                        [ w.name ++ ": 확인된 사실에는 근거가 필요합니다." ]

                    else
                        []
                   )
    in
    List.concatMap observation doc.observations ++ List.concatMap workflow doc.workflows
