# 아키텍처 문서

## MTL 스타일 리팩토링 개요

이 프로젝트는 Monomer GUI 애플리케이션을 MTL(Monad Transformer Library) 스타일로 리팩토링하여 SQLite 데이터베이스 연동을 추가했습니다.

## 레이어 구조

```
┌─────────────────────────────────────┐
│         UI Layer (Main.hs)          │
│  - Monomer Widgets                  │
│  - Event Handlers                   │
│  - Producers (비동기 IO)            │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│    Business Logic (TodoLogic.hs)    │
│  - Validation                       │
│  - Pure Functions                   │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│   Repository Interface (TodoRepo)   │
│  - MonadTodoRepo 타입클래스         │
│  - 추상 인터페이스                  │
└──────────────┬──────────────────────┘
               │
       ┌───────┴────────┐
       │                │
┌──────▼─────┐  ┌───────▼────────┐
│  SQLite    │  │  Mock (Test)   │
│ (Production)│  │  (Development) │
└────────────┘  └────────────────┘
```

## 모듈 설명

### 1. TodoTypes.hs
데이터 타입 정의:
- `Todo`: 할일 항목
- `TodoModel`: 애플리케이션 상태
- `TodoEvt`: 이벤트 타입
- `TodoAction`: UI 액션 상태

### 2. TodoRepo.hs
Repository 인터페이스:
```haskell
class Monad m => MonadTodoRepo m where
  getAllTodos :: m [Todo]
  insertTodo :: Todo -> m Todo
  updateTodo :: Int -> Todo -> m ()
  deleteTodo :: Int -> m ()
  initializeDb :: m ()
```

**장점:**
- 구현에 독립적인 인터페이스
- 테스트 시 Mock 구현으로 교체 가능
- 타입 안전성 보장

### 3. TodoRepoSqlite.hs
SQLite 구현:
```haskell
type AppM = ReaderT SqliteEnv IO

instance MonadTodoRepo AppM where
  -- SQLite 구체적 구현
```

**특징:**
- `ReaderT` 패턴으로 DB 연결 관리
- `FromRow`/`ToRow` 인스턴스로 자동 매핑
- Enum 타입을 Int로 변환하여 저장

### 4. TodoRepoMock.hs
테스트용 Mock 구현:
```haskell
type MockM = State [Todo]

instance MonadTodoRepo MockM where
  -- 메모리 기반 구현
```

**용도:**
- 단위 테스트
- 빠른 프로토타이핑
- DB 없이 로직 검증

### 5. TodoLogic.hs
비즈니스 로직:
```haskell
loadAllTodos :: (MonadTodoRepo m, MonadIO m) => m [Todo]
saveTodo :: (MonadTodoRepo m, MonadIO m) => Todo -> m Todo
validateTodo :: Todo -> Either Text Todo
```

**특징:**
- Repository 추상화에만 의존
- 순수 함수와 IO 함수 분리
- Validation 로직 포함

### 6. Main.hs
UI 및 이벤트 처리:
- Monomer 위젯 구성
- 이벤트 핸들러
- Producer 함수 (비동기 IO)

## 데이터 흐름

### 1. 애플리케이션 시작
```
main
  └─> withSqliteEnv "todos.db"
       └─> initializeDb
       └─> loadAllTodos (초기 데이터)
       └─> startApp
```

### 2. 할일 추가
```
User Click "Add"
  └─> TodoAdd Event
       └─> addTodoProducer
            └─> saveTodo (TodoLogic)
                 └─> insertTodo (TodoRepo)
                      └─> SQLite INSERT
            └─> loadAllTodos
            └─> TodosLoaded Event
                 └─> UI Update
```

### 3. 할일 수정
```
User Click "Save"
  └─> TodoSave Event
       └─> updateTodoProducer
            └─> updateExistingTodo (TodoLogic)
                 └─> updateTodo (TodoRepo)
                      └─> SQLite UPDATE
            └─> loadAllTodos
            └─> TodosLoaded Event
                 └─> UI Update
```

### 4. 할일 삭제
```
User Confirm Delete
  └─> TodoDelete Event
       └─> deleteTodoProducer
            └─> removeExistingTodo (TodoLogic)
                 └─> deleteTodo (TodoRepo)
                      └─> SQLite DELETE
            └─> loadAllTodos
            └─> TodosLoaded Event
                 └─> UI Update
```

## MTL 스타일의 장점

### 1. 테스트 용이성
```haskell
-- Production
runAppM sqliteEnv $ saveTodo todo

-- Test
runMockM [] $ saveTodo todo
```

### 2. 유연성
다른 데이터베이스로 쉽게 전환:
```haskell
-- PostgreSQL 구현
type PostgresM = ReaderT PostgresEnv IO
instance MonadTodoRepo PostgresM where ...

-- Redis 구현
type RedisM = ReaderT RedisEnv IO
instance MonadTodoRepo RedisM where ...
```

### 3. 관심사 분리
- UI는 비즈니스 로직만 호출
- 비즈니스 로직은 Repository 인터페이스만 사용
- Repository 구현은 독립적으로 변경 가능

### 4. 타입 안전성
컴파일 타임에 모든 의존성 체크:
```haskell
-- 컴파일 에러: MonadTodoRepo 제약 없음
badFunction :: IO [Todo]
badFunction = getAllTodos  -- Error!

-- 올바른 사용
goodFunction :: MonadTodoRepo m => m [Todo]
goodFunction = getAllTodos  -- OK
```

## 확장 가능성

### 1. 로깅 추가
```haskell
class Monad m => MonadLogger m where
  logInfo :: Text -> m ()
  logError :: Text -> m ()

-- 여러 제약 조합
saveTodoWithLog :: (MonadTodoRepo m, MonadLogger m) => Todo -> m Todo
saveTodoWithLog todo = do
  logInfo "Saving todo..."
  result <- saveTodo todo
  logInfo "Todo saved!"
  return result
```

### 2. 트랜잭션 지원
```haskell
class MonadTodoRepo m => MonadTodoTransaction m where
  withTransaction :: m a -> m a

-- 트랜잭션 내에서 여러 작업
bulkUpdate :: MonadTodoTransaction m => [Todo] -> m ()
bulkUpdate todos = withTransaction $ do
  mapM_ (\t -> updateTodo (fromIntegral $ _todoId t) t) todos
```

### 3. 캐싱 레이어
```haskell
class Monad m => MonadCache m where
  cacheGet :: Text -> m (Maybe a)
  cacheSet :: Text -> a -> m ()

-- 캐시를 사용하는 Repository
getCachedTodos :: (MonadTodoRepo m, MonadCache m) => m [Todo]
getCachedTodos = do
  cached <- cacheGet "todos"
  case cached of
    Just todos -> return todos
    Nothing -> do
      todos <- getAllTodos
      cacheSet "todos" todos
      return todos
```

## 성능 고려사항

### 1. Producer 패턴
Monomer의 Producer를 사용하여 비동기 IO:
```haskell
loadTodosProducer :: SqliteEnv -> (TodoEvt -> IO ()) -> IO ()
loadTodosProducer env sendMsg = do
  todos <- runAppM env loadAllTodos  -- 백그라운드 실행
  sendMsg (TodosLoaded todos)        -- UI 업데이트
```

### 2. 데이터베이스 연결 관리
`withSqliteEnv`로 자동 연결 관리:
```haskell
withSqliteEnv :: FilePath -> (SqliteEnv -> IO a) -> IO a
withSqliteEnv dbPath action = do
  conn <- open dbPath
  let env = SqliteEnv conn
  result <- action env
  close conn  -- 자동 정리
  return result
```

## 결론

MTL 스타일 리팩토링으로:
- ✅ 테스트 가능한 코드
- ✅ 유지보수 용이
- ✅ 확장 가능한 아키텍처
- ✅ 타입 안전성
- ✅ 관심사 분리

작은 프로젝트지만 확장 가능한 구조를 갖추었습니다.
