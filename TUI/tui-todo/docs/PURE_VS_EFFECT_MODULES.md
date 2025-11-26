# 순수 함수 vs 효과 모듈 분석

## 개요

TUI Todo 애플리케이션의 모듈을 순수(Pure) 코드와 효과(Effect) 코드로 분류하여 분석합니다.

## 순수(Pure) 모듈

순수 모듈은 부수효과가 없으며, 동일한 입력에 대해 항상 동일한 출력을 반환합니다.

### UI/Types.hs ✅ 완전히 순수

**내용**:
- 데이터 타입 정의
- 렌즈 생성 (Template Haskell)
- `fromTodoRow`: DB 행을 UI Todo로 변환하는 순수 함수

**예시**:
```haskell
data Mode = ViewMode | InputMode | EditMode DB.TodoId

fromTodoRow :: DB.TodoRow -> Todo
fromTodoRow row = Todo { ... }
```

**특징**: IO 없음, 상태 변경 없음

### UI/Attributes.hs ✅ 완전히 순수

**내용**:
- UI 속성 맵 정의
- 색상 및 스타일 설정

**예시**:
```haskell
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "header", V.white `on` V.blue)
  , (attrName "selected", V.black `on` V.cyan)
  , ...
  ]
```

**특징**: 순수한 설정 데이터

### UI/Draw.hs ✅ 완전히 순수

**내용**:
- UI 렌더링 함수들
- Brick의 `Widget` 타입 반환

**예시**:
```haskell
drawUI :: AppState -> [Widget Name]
drawHeader :: AppState -> Widget Name
drawTodo :: I18nMessages -> Bool -> Todo -> Widget Name
```

**특징**: 
- 부수효과 없음
- 입력 상태를 기반으로 UI 구조만 생성
- 테스트하기 쉬움

### I18n.hs (부분) ✅ 데이터 타입은 순수

**순수한 부분**:
- `Language` 데이터 타입
- `I18nMessages` 및 관련 데이터 타입들
- `defaultMessages` 상수

**예시**:
```haskell
data Language = English | Korean

defaultMessages :: I18nMessages
defaultMessages = I18nMessages { ... }
```

### Config.hs (부분) ✅ 일부 함수는 순수

**순수한 부분**:
- `keyToString`: Vty 키를 문자열로 변환
- `matchesKey`: 키가 특정 액션과 매칭되는지 확인

**예시**:
```haskell
keyToString :: V.Key -> String
matchesKey :: KeyBindings -> V.Key -> Maybe KeyAction
```

## 효과(Effect) 모듈

효과 모듈은 IO, 상태 변경, 데이터베이스 작업 등의 부수효과를 포함합니다.

### App.hs ❌ 완전히 효과

**내용**:
- MTL 기반 모나드 스택
- 데이터베이스 작업 래퍼

**예시**:
```haskell
newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }

loadTodos :: MonadApp m => m [DB.TodoRow]
createTodo :: MonadApp m => String -> m DB.TodoId
updateTodo :: MonadApp m => DB.TodoId -> String -> ... -> m ()
deleteTodo :: MonadApp m => DB.TodoId -> m ()
toggleTodo :: MonadApp m => DB.TodoId -> m ()
```

**효과**:
- `IO` 모나드 사용
- 데이터베이스 읽기/쓰기

### DB.hs ❌ 완전히 효과

**내용**:
- SQLite 데이터베이스 작업
- 시간 관련 IO 작업

**예시**:
```haskell
initDB :: Connection -> IO ()
createTodo :: Connection -> String -> IO TodoId
getAllTodos :: Connection -> IO [TodoRow]
updateTodo :: Connection -> TodoId -> ... -> IO ()
deleteTodo :: Connection -> TodoId -> IO ()
toggleTodoComplete :: Connection -> TodoId -> IO ()
formatCurrentTime :: IO String
```

**효과**:
- 데이터베이스 쿼리 실행
- 현재 시간 가져오기

### Config.hs (부분) ❌ 파일 I/O 포함

**효과가 있는 부분**:
- `loadKeyBindings`: YAML 파일 읽기
- `loadKeyBindingsWithMessages`: 파일 읽기 + 메시지 출력

**예시**:
```haskell
loadKeyBindings :: IO KeyBindings
loadKeyBindings = do
  exists <- doesFileExist path
  if exists
    then loadFromFile path
    else useDefault
```

**효과**:
- 파일 시스템 접근
- 파일 읽기
- YAML 파싱
- 콘솔 출력

### I18n.hs (부분) ❌ 파일 I/O 포함

**효과가 있는 부분**:
- `loadMessages`: YAML 파일에서 메시지 로드

**예시**:
```haskell
loadMessages :: Language -> IO I18nMessages
loadMessages lang = do
  let path = case lang of ...
  exists <- doesFileExist path
  if exists
    then loadFromFile path
    else useDefault
```

**효과**:
- 파일 시스템 접근
- 파일 읽기
- YAML 파싱
- 콘솔 출력

### UI/Events.hs ❌ 완전히 효과

**내용**:
- 이벤트 핸들러
- 상태 변경
- 데이터베이스 작업 호출

**예시**:
```haskell
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
toggleTodoComplete :: EventM Name AppState ()
deleteTodo :: EventM Name AppState ()
saveNewTodo :: EventM Name AppState ()
```

**효과**:
- `EventM` 모나드 사용 (상태 변경)
- `liftIO`로 IO 작업 수행
- 데이터베이스 작업 호출

### Lib.hs ❌ 효과 포함

**내용**:
- Brick 애플리케이션 정의
- 이벤트 핸들러 연결

**예시**:
```haskell
app :: App AppState e Name
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent  -- 효과 포함
  , ...
  }
```

**효과**:
- `appHandleEvent`가 효과를 포함하는 함수 참조

## 분류 요약

### 순수 모듈 (3개)
1. ✅ `UI/Types.hs` - 데이터 타입 정의
2. ✅ `UI/Attributes.hs` - 스타일 정의
3. ✅ `UI/Draw.hs` - UI 렌더링

### 효과 모듈 (5개)
1. ❌ `App.hs` - MTL 모나드, DB 작업
2. ❌ `DB.hs` - SQLite 작업
3. ❌ `Config.hs` - 파일 I/O
4. ❌ `I18n.hs` - 파일 I/O
5. ❌ `UI/Events.hs` - 이벤트 처리, 상태 변경

### 혼합 모듈 (1개)
- ⚠️ `Lib.hs` - 순수 함수 재export + 효과 통합

## 설계 원칙

### 순수 함수의 장점
1. **테스트 용이**: 입력만 제공하면 출력 검증 가능
2. **예측 가능**: 동일 입력 → 동일 출력
3. **병렬화 가능**: 부수효과 없어 안전
4. **재사용 용이**: 어디서든 사용 가능

### 효과 분리의 장점
1. **명확한 책임**: 어디서 부수효과가 발생하는지 명확
2. **테스트 전략**: 순수 함수는 단위 테스트, 효과는 통합 테스트
3. **유지보수**: 효과를 최소화하고 격리

## 아키텍처 패턴

### Functional Core, Imperative Shell

```
┌─────────────────────────────────────┐
│     Imperative Shell (Effects)      │
│  ┌───────────────────────────────┐  │
│  │   Functional Core (Pure)     │  │
│  │                               │  │
│  │  UI/Types.hs                 │  │
│  │  UI/Attributes.hs            │  │
│  │  UI/Draw.hs                  │  │
│  │                               │  │
│  └───────────────────────────────┘  │
│                                     │
│  App.hs, DB.hs, Config.hs          │
│  I18n.hs, UI/Events.hs             │
└─────────────────────────────────────┘
```

### 데이터 흐름

```
User Input
    ↓
UI/Events.hs (Effect)
    ↓
App.hs (Effect) → DB.hs (Effect)
    ↓
AppState (Pure Data)
    ↓
UI/Draw.hs (Pure)
    ↓
Screen Output
```

## 테스트 전략

### 순수 모듈 테스트
```haskell
-- UI/Draw.hs 테스트 예시
testDrawTodo :: Test
testDrawTodo = do
  let todo = Todo { ... }
  let widget = drawTodo defaultMessages False todo
  -- widget 구조 검증
```

### 효과 모듈 테스트
```haskell
-- App.hs 테스트 예시 (Mock 사용)
testCreateTodo :: Test
testCreateTodo = do
  conn <- openTestDB
  result <- runAppM (AppEnv conn msgs) $ createTodo "Test"
  -- 결과 검증
  closeTestDB conn
```

## 결론

TUI Todo 애플리케이션은 순수 함수와 효과를 명확히 분리하여 설계되었습니다:

- **UI 렌더링**: 완전히 순수
- **데이터 타입**: 순수
- **비즈니스 로직**: 효과 (MTL로 추상화)
- **I/O 작업**: 효과 (명시적으로 격리)

이러한 분리는 테스트 가능성, 유지보수성, 확장성을 크게 향상시킵니다.
