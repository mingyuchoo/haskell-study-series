{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.Lens qualified as L
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import TextShow

-- 1. 데이터 모델 정의 (상태)
newtype AppModel = AppModel {_clickCount :: Int}
  deriving (Eq, Show)

-- Lens 생성 (상태 업데이트를 쉽게 하기 위함)
makeLenses 'AppModel

-- 2. 이벤트 정의 (사용자 액션)
data AppEvent = AppInit | AppIncrease
  deriving (Eq, Show)

-- 3. UI 빌드 (화면 구성)
buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Hello world",
          spacer,
          hstack
            [ label $ "Click count: " <> showt (model ^. clickCount),
              spacer,
              button "Increase count" AppIncrease
            ]
        ]
        `styleBasic` [padding 10]

-- 4. 이벤트 핸들러 (로직 처리)
handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]

-- 5. 메인 함수

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hello world",
        appWindowIcon "./assets/images/icon.png",
        appTheme darkTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model = AppModel 0
