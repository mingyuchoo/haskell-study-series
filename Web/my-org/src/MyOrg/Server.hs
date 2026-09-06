module MyOrg.Server (application) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai
import MyOrg.Types
import MyOrg.Demo (isDemoEpoch)
import MyOrg.Application
import MyOrg.Store
import MyOrg.Registry
import MyOrg.Domain.Event
import MyOrg.Domain.Evaluation
import MyOrg.Domain.Compiler
import MyOrg.Domain.Graph
import MyOrg.Domain.Analysis (analyzeGoal)
import MyOrg.Domain.Review

application :: Store -> Application
application store request respond
  | requestMethod request == methodGet && pathInfo request `elem` [[], ["app.js"], ["style.css"]] =
      let (path, mime) = case pathInfo request of
            ["app.js"] -> ("static/app.js", "text/javascript; charset=utf-8")
            ["style.css"] -> ("static/style.css", "text/css; charset=utf-8")
            _ -> ("static/index.html", "text/html; charset=utf-8")
       in respond (responseFile status200 [(hContentType,mime),("Content-Security-Policy","default-src 'self'; style-src 'self'; script-src 'self'; frame-ancestors 'none'"),("X-Content-Type-Options","nosniff")] path Nothing)
  | requestMethod request == methodGet = do
      registry <- readRegistry store
      now <- getCurrentTime
      case readProjection now registry (pathInfo request) of
        Left err -> failure (errorStatus err) (describeError err)
        Right Nothing -> failure status404 "경로를 찾을 수 없습니다."
        Right (Just value) -> json status200 value
  | requestMethod request `elem` [methodPost, methodPatch, methodDelete] = do
      if lookup hContentType (requestHeaders request) /= Just "application/json"
        then failure status415 "Content-Type: application/json이 필요합니다."
        else do
          body <- limitedBody request
          case body >>= eitherDecode of
            Left message -> failure status400 (T.pack message)
            Right (Object _) | requestMethod request == methodPost && pathInfo request == ["api", "demo"] -> do
              outcome <- seedDemo store
              result status201 outcome
            Right value -> case writeRoute (requestMethod request) (pathInfo request) of
              Nothing -> failure status404 "경로를 찾을 수 없습니다."
              Just (scope, parser) -> case parseEither parser value of
                Left message -> failure status400 (T.pack message)
                Right (actor, command) -> do
                  outcome <- maybe (runCommand store actor command) (\oid -> runOrganizationCommand store oid actor command) scope
                  result (if requestMethod request == methodPost then status201 else status200) outcome
  | otherwise = failure status405 "지원하지 않는 메서드입니다."
 where
  json :: Status -> Value -> IO ResponseReceived
  json status value = respond (responseLBS status [(hContentType,"application/json; charset=utf-8"),("Cache-Control","no-store")] (encode value))
  failure :: Status -> Text -> IO ResponseReceived
  failure status message = json status (object ["error" .= message])
  result status = either (\err -> failure (errorStatus err) (describeError err)) (\events -> json status (object ["events" .= events]))

-- Registry routes are independent of organization selection. All other reads
-- resolve exactly one explicit scope (or the sole active legacy organization).
readProjection :: UTCTime -> Registry -> [Text] -> Either OrganizationError (Maybe Value)
readProjection now registry path = case path of
  ["api", "organizations"] -> pure (Just (toJSON [summary registry st | st <- activeOrganizations registry]))
  ["api", "organizations", oid] -> Just . summary registry <$> organizationState registry (OrgId oid)
  ["api", "organizations", oid, resource] | resource `elem` resources -> do
    st <- organizationState registry (OrgId oid)
    pure (Just (project now registry st resource))
  ["api", resource] | resource `elem` resources -> do
    target <- resolveSingleOrganization registry
    st <- maybe (pure emptyState {stateLastSeq = registryLastSeq registry}) (organizationState registry) target
    pure (Just (project now registry st resource))
  _ -> pure Nothing
 where
  resources = ["dashboard", "organization", "people", "goals", "compiler", "graph", "events", "reviews"]

summary :: Registry -> OrgState -> Value
summary registry st = object
  [ "organization" .= stateOrganization st, "version" .= stateLastSeq st
  , "demo" .= isDemoEpoch (stream registry st)
  , "peopleCount" .= Map.size (statePeople st), "goalCount" .= Map.size (stateGoals st)
  ]

stream :: Registry -> OrgState -> [StoredEvent]
stream registry st = maybe [] (\org -> Map.findWithDefault [] (organizationId org) (registryEvents registry)) (stateOrganization st)

project :: UTCTime -> Registry -> OrgState -> Text -> Value
project now registry st resource = case resource of
  "dashboard" -> object
    [ "version" .= stateLastSeq st, "demo" .= isDemoEpoch history, "organization" .= stateOrganization st
    , "people" .= Map.elems (statePeople st), "goals" .= goals, "authorities" .= Map.elems (stateAuthorities st)
    , "compiler" .= compileOrganization now st, "graph" .= buildGraph st, "decisionShare" .= decisionShare st
    , "reviews" .= stateReviews st
    , "reviewWarnings" .= [object ["id" .= reviewId r, "warnings" .= map describeReviewWarning (checkReview r)] | r <- stateReviews st]
    , "events" .= [object ["record" .= e, "description" .= describeEvent (storedEvent e)] | e <- reverse events]
    ]
  "organization" -> toJSON (stateOrganization st)
  "people" -> toJSON (Map.elems (statePeople st))
  "goals" -> toJSON goals
  "compiler" -> toJSON (compileOrganization now st)
  "graph" -> toJSON (buildGraph st)
  "events" -> toJSON events
  "reviews" -> toJSON (stateReviews st)
  _ -> Null
 where
  history = stream registry st
  events = currentEpoch history
  goals = [object
    [ "goal" .= g, "owner" .= goalOwner st (goalId g), "active" .= Set.member (goalId g) (stateActive st)
    , "evaluation" .= evaluateGoal now g (resultsOf st (goalId g))
    , "analysis" .= either (const Null) toJSON (analyzeGoal st (goalId g))
    , "results" .= resultsOf st (goalId g), "strategies" .= Map.findWithDefault [] (goalId g) (stateStrategies st)
    ] | g <- Map.elems (stateGoals st)]

writeRoute :: Method -> [Text] -> Maybe (Maybe OrgId, Value -> Parser (Maybe UserId, Command))
writeRoute method path = case (method, path) of
  ("POST", ["api", "demo"]) -> Just (Nothing, const (fail "데모 요청은 JSON 객체여야 합니다."))
  ("POST", ["api", "organizations"]) -> Just (Nothing, parseCommand path)
  ("PATCH", ["api", "organizations", oid]) -> Just (Just (OrgId oid), withObject "rename" $ \o ->
    (,) <$> o .:? "actor" <*> (RenameOrganization (OrgId oid) <$> o .: "name" <*> o .: "expectedVersion"))
  ("DELETE", ["api", "organizations", oid]) -> Just (Just (OrgId oid), withObject "delete" $ \o ->
    (,) <$> o .:? "actor" <*> (DeleteOrganization (OrgId oid) <$> o .: "confirmName" <*> o .: "expectedVersion"))
  ("POST", "api":"organizations":oid:rest) | knownCommand ("api":rest) -> Just (Just (OrgId oid), parseCommand ("api":rest))
  ("POST", _) | knownCommand path -> Just (Nothing, parseCommand path)
  _ -> Nothing

knownCommand :: [Text] -> Bool
knownCommand path = case path of
  ["api", name] -> name `elem` ["people", "goals", "evaluations", "reviews"]
  ["api", "goals", _, action] -> action `elem` ["owner", "authority", "activate", "results", "strategy"]
  ["api", "people", _, "authority"] -> True
  _ -> False

parseCommand :: [Text] -> Value -> Parser (Maybe UserId, Command)
parseCommand path = withObject "command" $ \o -> do
  actor <- o .:? "actor"
  command <- case path of
    ["api","organizations"] -> CreateOrganization <$> o .: "id" <*> o .: "name"
    ["api","people"] -> AddPerson <$> parseJSON (Object o)
    ["api","goals"] -> CreateGoal <$> parseJSON (Object o)
    ["api","goals",gid,"owner"] -> AssignOwner (GoalId gid) <$> o .: "owner"
    ["api","people",uid,"authority"] -> do
      a <- parseJSON (Object o)
      if authorityOwner a /= UserId uid then fail "권한 소유자가 경로와 일치하지 않습니다." else pure (GrantAuthority a)
    ["api","goals",gid,"authority"] -> GrantGoalAuthority (GoalId gid) <$> parseJSON (Object o)
    ["api","goals",gid,"activate"] -> pure (ActivateGoal (GoalId gid))
    ["api","goals",gid,"results"] -> ReportResult (GoalId gid) <$> o .: "value" <*> o .: "reportedBy" <*> o .: "note"
    ["api","evaluations"] -> EvaluateGoal <$> o .: "goal"
    ["api","reviews"] -> HoldReview <$> o .: "id" <*> o .: "goal" <*> o .:? "learnings" .!= [] <*> o .:? "decisions" .!= [] <*> o .: "note"
    ["api","goals",gid,"strategy"] -> ChangeStrategy (GoalId gid) <$> o .: "note"
    _ -> fail "경로를 찾을 수 없습니다."
  pure (actor, command)

limitedBody :: Request -> IO (Either String BL.ByteString)
limitedBody request = go 0 []
 where
  go n chunks = do
    chunk <- getRequestBodyChunk request
    let size = n + BS.length chunk
    if size > 1048576 then pure (Left "요청은 1MB 이하여야 합니다.")
    else if BS.null chunk then pure (Right (BL.fromChunks (reverse chunks)))
    else go size (chunk:chunks)

errorStatus :: OrganizationError -> Status
errorStatus = \case
  AmbiguousOrganizations -> status409
  OrganizationNotFound _ -> status404
  VersionConflict _ _ -> status409
  StorageFailure -> status500
  GoalNotFound _ -> status404
  PersonNotFound _ -> status404
  DuplicateId _ -> status409
  OrganizationAlreadyExists -> status409
  GoalAlreadyActive _ -> status409
  GoalNotActive _ -> status409
  _ -> status400
