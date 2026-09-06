module Domain exposing (..)

import Dict exposing (Dict)


type Status
    = NoData
    | OnTrack
    | AtRisk
    | OffTrack
    | Achieved


type alias Organization =
    { id : String, name : String, createdAt : String }


type alias Summary =
    { organization : Organization, demo : Bool, peopleCount : Int, goalCount : Int }


type alias Person =
    { id : String, name : String, role : String, reportsTo : Maybe String, department : Maybe String, email : Maybe String, active : Bool }


type alias Metric =
    { id : String, name : String, unit : String, direction : String }


type alias Goal =
    { id : String, description : String, metric : Metric, baseline : Float, target : Float, deadline : String, requiredBudget : Float, requiredPermissions : List String }


type alias Evaluation =
    { status : Status, progress : Float, latestValue : Maybe Float }


type alias Analysis =
    { coverage : Float, possibleCause : String }


type alias Measurement =
    { value : Float, reportedAt : String, note : String, reportedBy : Maybe String }


type alias GoalView =
    { goal : Goal, owner : Maybe String, active : Bool, evaluation : Evaluation, analysis : Analysis, results : List Measurement, strategies : List ( String, String ) }


type alias Authority =
    { owner : String, budgetLimit : Float, canHire : Bool, canChangePrice : Bool, canApprove : List String }


type alias Decision =
    { text : String, owner : String, deadline : Maybe String }


type alias Review =
    { id : String, goal : String, heldAt : String, note : String, evaluation : Evaluation, learnings : List String, decisions : List Decision }


type alias Diagnostic =
    { severity : String, code : String, message : String, subject : String, details : List String }


type alias Compiler =
    { errors : Int, warnings : Int, diagnostics : List Diagnostic }


type alias Node =
    { tag : String, contents : String }


type alias Edge =
    { from : Node, to : Node, kind : String }


type alias Audit =
    { seq : Int, at : String, actor : Maybe String, description : String, evaluatedGoal : Maybe String, evaluatedStatus : Maybe Status }


type alias ReviewWarning =
    { id : String, warnings : List String }


type alias Workspace =
    { organization : Organization, version : Int, demo : Bool, people : List Person, goals : List GoalView, authorities : List Authority, reviews : List Review, compiler : Compiler, edges : List Edge, events : List Audit, decisionShare : Dict String Float, reviewWarnings : List ReviewWarning }
