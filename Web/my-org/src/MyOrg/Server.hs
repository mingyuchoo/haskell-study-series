module MyOrg.Server
  ( application
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import MyOrg.Application.Query (executeQuery)
import MyOrg.Http.Encode
import MyOrg.Http.Route
import MyOrg.Presentation.Error
import MyOrg.Serialization.JSON (toWire)
import MyOrg.Store
import Network.HTTP.Types
import Network.Wai

application :: Store -> Application
application store request respond
  | requestMethod request == methodGet
      && pathInfo request `elem` [[], ["app.js"], ["bootstrap.js"], ["style.css"]] =
      let (path, mime) = case pathInfo request of
            ["app.js"]       -> ("static/app.js", "text/javascript; charset=utf-8")
            ["bootstrap.js"] -> ("static/bootstrap.js", "text/javascript; charset=utf-8")
            ["style.css"]    -> ("static/style.css", "text/css; charset=utf-8")
            _                -> ("static/index.html", "text/html; charset=utf-8")
       in respond
            ( responseFile
                status200
                [ (hContentType, mime)
                ,
                  ( "Content-Security-Policy"
                  , "default-src 'self'; style-src 'self'; script-src 'self'; frame-ancestors 'none'"
                  )
                , ("X-Content-Type-Options", "nosniff")
                ]
                path
                Nothing
            )
  | requestMethod request == methodGet = do
      registry <- readRegistry store
      now <- getCurrentTime
      case readRoute (pathInfo request) of
        Nothing -> failure status404 "경로를 찾을 수 없습니다."
        Just query -> case executeQuery now registry query of
          Left err    -> failure (errorStatus err) (describeError err)
          Right value -> json status200 (encodeQueryResult value)
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
                  outcome <-
                    maybe
                      (runCommand store actor command)
                      (\oid -> runOrganizationCommand store oid actor command)
                      scope
                  result (if requestMethod request == methodPost then status201 else status200) outcome
  | otherwise = failure status405 "지원하지 않는 메서드입니다."
  where
    json :: Status -> Value -> IO ResponseReceived
    json status value =
      respond
        ( responseLBS
            status
            [(hContentType, "application/json; charset=utf-8"), ("Cache-Control", "no-store")]
            (encode value)
        )
    failure :: Status -> Text -> IO ResponseReceived
    failure status message = json status (object ["error" .= message])
    result status =
      either
        (\err -> failure (errorStatus err) (describeError err))
        (\events -> json status (object ["events" .= toWire events]))

limitedBody :: Request -> IO (Either String BL.ByteString)
limitedBody request = go 0 []
  where
    go n chunks = do
      chunk <- getRequestBodyChunk request
      let size = n + BS.length chunk
      if size > 1048576
        then pure (Left "요청은 1MB 이하여야 합니다.")
        else
          if BS.null chunk
            then pure (Right (BL.fromChunks (reverse chunks)))
            else go size (chunk : chunks)
