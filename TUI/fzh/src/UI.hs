module UI
    ( drawUI
    , handleEvent
    , theApp
    ) where

import Brick
import Brick.Widgets.Border (border, borderWithLabel, hBorder)
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Lens.Micro ((^.), (&), (.~), (%~))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import AppState
import ParallelFilter
import qualified Cache as C

-- | UI 렌더링
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui = vBox
        [ drawHeader
        , hBorder
        , drawQueryEditor st
        , hBorder
        , drawResults st
        , hBorder
        , drawFooter st
        ]

drawHeader :: Widget Name
drawHeader = hCenter $ str "FZF in Haskell - Fuzzy Finder"

drawQueryEditor :: AppState -> Widget Name
drawQueryEditor st = 
    borderWithLabel (str " Query ") $
    E.renderEditor (txt . T.unlines) True (st ^. queryEditor)

drawResults :: AppState -> Widget Name
drawResults st = 
    borderWithLabel (str " Results ") $
    L.renderList drawResult True (st ^. resultList)

drawResult :: Bool -> FilterResult -> Widget Name
drawResult selected result = 
    let style = if selected then withAttr (attrName "selected") else id
        text = filterText result
        score = filterScore result
    in style $ txt text <+> str " " <+> str (show score)

drawFooter :: AppState -> Widget Name
drawFooter st = 
    let count = Vec.length $ L.listElements (st ^. resultList)
        total = Vec.length (st ^. allItems)
    in hCenter $ str $ show count ++ "/" ++ show total

-- | 이벤트 핸들러
handleEvent :: C.Cache T.Text [FilterResult] -> BrickEvent Name e -> EventM Name AppState ()
handleEvent cache (VtyEvent ev) = do
    st <- get
    case ev of
        V.EvKey V.KEsc [] -> halt
        V.EvKey V.KEnter [] -> do
            -- 선택된 항목 출력하고 종료
            case L.listSelectedElement (st ^. resultList) of
                Just (_, result) -> do
                    liftIO $ putStrLn $ T.unpack $ filterText result
                    halt
                Nothing -> return ()
        
        V.EvKey (V.KChar 'c') [V.MCtrl] -> halt
        
        -- 결과 리스트 네비게이션
        V.EvKey V.KUp [] -> do
            let newList = L.listMoveUp (st ^. resultList)
            modify $ \s -> s & resultList .~ newList
        
        V.EvKey V.KDown [] -> do
            let newList = L.listMoveDown (st ^. resultList)
            modify $ \s -> s & resultList .~ newList
        
        -- 쿼리 에디터 이벤트
        _ -> do
            zoom queryEditor $ E.handleEditorEvent (VtyEvent ev)
            newSt <- get
            let newQuery = T.concat $ E.getEditContents (newSt ^. queryEditor)
            
            when (newQuery /= (st ^. currentQuery)) $ do
                -- 캐시 확인
                cached <- liftIO $ C.lookup newQuery cache
                case cached of
                    Just results -> do
                        modify $ updateResults results . updateQuery newQuery
                    Nothing -> do
                        -- 병렬 필터링 수행
                        results <- liftIO $ parallelFilter newQuery (newSt ^. allItems)
                        liftIO $ C.insert newQuery results cache
                        modify $ updateResults results . updateQuery newQuery

handleEvent _ _ = return ()

-- | 속성 맵
theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrName "selected", V.black `on` V.white)
    ]

-- | Brick 애플리케이션
theApp :: C.Cache T.Text [FilterResult] -> App AppState e Name
theApp cache = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent cache
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }
