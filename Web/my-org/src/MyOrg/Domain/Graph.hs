-- | Responsibility Graph. 조직을 트리가 아니라 그래프로 본다.
--
-- > Person --Owns--> Goal
-- > Goal   --DependsOn--> Goal (하위 목표)
-- > Goal   --Measures--> Metric
-- > Person --Controls--> Resource (권한 또는 예산)
module MyOrg.Domain.Graph
  ( Node (..)
  , EdgeKind (..)
  , Edge (..)
  , ResponsibilityGraph (..)
  , buildGraph
  , resourceOfPermission
  , budgetResource
  , goalsWithoutOwner
  , ownersLackingAuthority
  , sharedMetricOwners
  , decisionShare
  , resourceControllers
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import MyOrg.Domain.State
import MyOrg.Domain.Goal (authorityCoverage)
import MyOrg.Domain.Identity
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Authority

data Node
  = PersonNode UserId
  | GoalNode GoalId
  | MetricNode MetricId
  | ResourceNode ResourceId
  deriving stock (Show, Eq, Ord, Generic)

data EdgeKind = Owns | DependsOn | Controls | Measures
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data Edge = Edge
  { edgeFrom :: Node
  , edgeKind :: EdgeKind
  , edgeTo :: Node
  }
  deriving stock (Show, Eq, Ord, Generic)



data ResponsibilityGraph = ResponsibilityGraph
  { graphNodes :: Set Node
  , graphEdges :: [Edge]
  }
  deriving stock (Show, Eq, Generic)



resourceOfPermission :: Permission -> ResourceId
resourceOfPermission = ResourceId . T.pack . show

budgetResource :: ResourceId
budgetResource = ResourceId "Budget"

buildGraph :: OrgState -> ResponsibilityGraph
buildGraph st = ResponsibilityGraph nodes edges
 where
  people = Map.keys (statePeople st)
  goals = Map.elems (stateGoals st)
  ownsEdges =
    [ Edge (PersonNode (ownershipOwner o)) Owns (GoalNode gid)
    | (gid, o) <- Map.toList (stateOwnership st)
    ]
  dependsEdges =
    [ Edge (GoalNode p) DependsOn (GoalNode (goalId g))
    | g <- goals
    , Just p <- [goalParent g]
    ]
  measuresEdges =
    [Edge (GoalNode (goalId g)) Measures (MetricNode (metricId (goalMetric g))) | g <- goals]
  controlsEdges =
    concat
      [ [Edge (PersonNode uid) Controls (ResourceNode (resourceOfPermission p))
        | p <- Set.toList (grantedPermissions a)
        ]
          ++ [Edge (PersonNode uid) Controls (ResourceNode budgetResource)
             | authorityBudgetLimit a > Money 0
             ]
      | (uid, a) <- Map.toList (stateAuthorities st)
      ]
  edges = ownsEdges ++ dependsEdges ++ measuresEdges ++ controlsEdges
  nodes =
    Set.unions
      [ Set.fromList (map PersonNode people)
      , Set.fromList (map (GoalNode . goalId) goals)
      , Set.fromList (concatMap (\e -> [edgeFrom e, edgeTo e]) edges)
      ]

-- | 책임자가 없는 목표.
goalsWithoutOwner :: OrgState -> [Goal]
goalsWithoutOwner st =
  [g | g <- Map.elems (stateGoals st), not (Map.member (goalId g) (stateOwnership st))]

-- | 책임은 있는데 권한이 부족한 사람. 목표별 통제 비율과 함께 돌려준다.
ownersLackingAuthority :: OrgState -> [(Goal, UserId, Double)]
ownersLackingAuthority st =
  [ (g, uid, coverage)
  | (gid, o) <- Map.toList (stateOwnership st)
  , Just g <- [Map.lookup gid (stateGoals st)]
  , let uid = ownershipOwner o
  , let coverage = maybe 0 (authorityCoverage g) (Map.lookup uid (stateAuthorities st))
  , coverage < 1
  ]

-- | 같은 지표를 서로 다른 사람이 최종 책임지는 경우.
sharedMetricOwners :: OrgState -> [(MetricId, [(GoalId, UserId)])]
sharedMetricOwners st =
  [ (mid, owners)
  | (mid, owners) <- Map.toList byMetric
  , Set.size (Set.fromList (map snd owners)) > 1
  ]
 where
  byMetric =
    Map.fromListWith
      (++)
      [ (metricId (goalMetric g), [(goalId g, ownershipOwner o)])
      | (gid, o) <- Map.toList (stateOwnership st)
      , Just g <- [Map.lookup gid (stateGoals st)]
      ]

-- | 전체 의사결정 권한 중 각 사람이 차지하는 비율.
--
-- 권한 하나를 1점, 예산 보유를 1점으로 센다.
decisionShare :: OrgState -> Map UserId Double
decisionShare st
  | total == 0 = Map.empty
  | otherwise = Map.map (\n -> fromIntegral n / fromIntegral total) points
 where
  points :: Map UserId Int
  points =
    Map.map
      (\a -> Set.size (grantedPermissions a) + (if authorityBudgetLimit a > Money 0 then 1 else 0))
      (stateAuthorities st)
  total = sum (Map.elems points)

-- | 특정 권한을 실제로 쥐고 있는 사람들.
resourceControllers :: OrgState -> Permission -> [UserId]
resourceControllers st p =
  [uid | (uid, a) <- Map.toList (stateAuthorities st), hasPermission a p]
