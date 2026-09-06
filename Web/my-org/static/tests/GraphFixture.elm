module GraphFixture exposing (edge, goal, node, sample, workspace)

import Dict
import Domain exposing (..)


workspace : Workspace
workspace =
    { organization = { id = "org-a", name = "검증 조직", createdAt = "2026-01-01" }, version = 1, demo = False, people = [], goals = [], authorities = [], reviews = [], compiler = { errors = 0, warnings = 0, diagnostics = [] }, edges = [], events = [], decisionShare = Dict.empty, reviewWarnings = [] }


node : String -> String -> Node
node tag id =
    { tag = tag, contents = id }


edge : String -> Node -> Node -> Edge
edge kind from to =
    { from = from, to = to, kind = kind }


goal : GoalView
goal =
    { goal = { id = "g", description = "고객 성장", metric = { id = "m", name = "신규 고객 수", unit = "명", direction = "HigherIsBetter" }, baseline = 0, target = 100, deadline = "2026-12-31", requiredBudget = 100, requiredPermissions = [ "Hiring" ] }, owner = Just "p", active = False, evaluation = { status = NoData, progress = 0, latestValue = Nothing }, analysis = { coverage = 0.5, possibleCause = "권한 부족" }, results = [], strategies = [] }


sample : Workspace
sample =
    let
        person id =
            { id = id, name = "같은 이름", role = "운영", department = Nothing, email = Nothing, reportsTo = Nothing, active = True }

        base =
            goal.goal

        isolated =
            { goal | goal = { base | id = "isolated", description = "연결 없는 목표" }, owner = Nothing }
    in
    { workspace
        | people = [ person "p", person "p2" ]
        , goals = [ goal, isolated ]
        , authorities = [ { owner = "p", budgetLimit = 50, canHire = False, canChangePrice = True, canApprove = [] } ]
        , edges =
            [ edge "Owns" (node "PersonNode" "p") (node "GoalNode" "g")
            , edge "Owns" (node "PersonNode" "p") (node "GoalNode" "g")
            , edge "Measures" (node "GoalNode" "g") (node "MetricNode" "m")
            , edge "DependsOn" (node "GoalNode" "g") (node "GoalNode" "isolated")
            , edge "Controls" (node "PersonNode" "p") (node "ResourceNode" "Budget")
            , edge "Controls" (node "PersonNode" "p") (node "ResourceNode" "Pricing")
            ]
    }
