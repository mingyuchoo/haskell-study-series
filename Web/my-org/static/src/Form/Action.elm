module Form.Action exposing (Action(..), actionKey)


type Action
    = CreateOrg
    | ImportDemo
    | Rename
    | AddPerson
    | AddGoal
    | Assign String
    | Grant String
    | Report String
    | Strategy String
    | AddReview
    | Activate String
    | Evaluate String
    | DeleteOrg


actionKey : Action -> String
actionKey action =
    case action of
        CreateOrg ->
            "organization"

        ImportDemo ->
            "demo"

        Rename ->
            "rename"

        AddPerson ->
            "person"

        AddGoal ->
            "goal"

        Assign key ->
            "owner-" ++ key

        Grant key ->
            "authority-" ++ key

        Report key ->
            "result-" ++ key

        Strategy key ->
            "strategy-" ++ key

        AddReview ->
            "review"

        Activate key ->
            "activate-" ++ key

        Evaluate key ->
            "evaluate-" ++ key

        DeleteOrg ->
            "delete"
