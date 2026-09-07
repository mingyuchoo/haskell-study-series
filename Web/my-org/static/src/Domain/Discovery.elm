module Domain.Discovery exposing (Change(..), Document, Observation, Review, Snapshot, Workflow, apply, empty, problems, statusLabel)


type alias Observation =
    { id : String, subject : String, detail : String, status : String, evidence : String }


type alias Workflow =
    { id : String, name : String, role : String, trigger : String, inputs : String, tools : String, outputs : String, handoff : String, approval : String, status : String, evidence : String }


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
    | RemoveObservation String
    | RemoveWorkflow String
    | ReviewNote String
    | ReviewStatus String


empty : Document
empty =
    { scope = "", asOf = "", observations = [], workflows = [], review = { status = "pending", note = "" } }


statusLabel : String -> String
statusLabel status =
    case status of
        "confirmed" ->
            "확인된 사실"

        "proposed" ->
            "개선안"

        _ ->
            "미확인"


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
                    { doc | workflows = doc.workflows ++ [ { id = ident, name = "", role = "", trigger = "", inputs = "", tools = "", outputs = "", handoff = "", approval = "", status = "unknown", evidence = "" } ] }

                WorkflowField ident key value ->
                    { doc
                        | workflows =
                            List.map
                                (\w ->
                                    if w.id == ident then
                                        editWorkflow key value w

                                    else
                                        w
                                )
                                doc.workflows
                    }

                RemoveObservation ident ->
                    { doc | observations = List.filter (\o -> o.id /= ident) doc.observations }

                RemoveWorkflow ident ->
                    { doc | workflows = List.filter (\w -> w.id /= ident) doc.workflows }

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
