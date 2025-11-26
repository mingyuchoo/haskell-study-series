module AppState
    ( AppState(..)
    , Name(..)
    , initialState
    , updateQuery
    , updateResults
    , queryEditor
    , resultList
    , allItems
    , currentQuery
    , isLoading
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Lens.Micro ((&), (.~))
import Lens.Micro.TH (makeLenses)
import ParallelFilter (FilterResult)

-- | 위젯 이름
data Name = QueryEditor | ResultList
    deriving (Eq, Ord, Show)

-- | 애플리케이션 상태
data AppState = AppState
    { _queryEditor :: E.Editor T.Text Name
    , _resultList :: L.List Name FilterResult
    , _allItems :: V.Vector T.Text
    , _currentQuery :: T.Text
    , _isLoading :: Bool
    } deriving (Show)

makeLenses ''AppState

-- | 초기 상태 생성
initialState :: V.Vector T.Text -> AppState
initialState items = AppState
    { _queryEditor = E.editor QueryEditor (Just 1) ""
    , _resultList = L.list ResultList (V.fromList []) 1
    , _allItems = items
    , _currentQuery = ""
    , _isLoading = False
    }

-- | 쿼리 업데이트
updateQuery :: T.Text -> AppState -> AppState
updateQuery query st = st & currentQuery .~ query

-- | 결과 업데이트
updateResults :: [FilterResult] -> AppState -> AppState
updateResults results st = 
    st & resultList .~ L.list ResultList (V.fromList results) 1
