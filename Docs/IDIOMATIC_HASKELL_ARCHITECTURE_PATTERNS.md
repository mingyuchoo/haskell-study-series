# Idiomatic Haskell Architecture Patterns

## 하스켈의 아키텍처 패턴(MTL, Free Monad, Tagless Final)의 차이도 비교

### 3가지 패턴 요약 비교

| 패턴                | 핵심 개념                            | 장점                 | 단점                  | 적합한 경우                          |
| ----------------- | -------------------------------- | ------------------ | ------------------- | ------------------------------- |
| **MTL**           | 모나드 스택 + 타입클래스 능력                | 단순, 빠름, 실용적        | 스택 의존성, 타입 오류 복잡    | 중소 규모 프로젝트, 일반적인 Haskell        |
| **Free Monad**    | 프로그램을 “명령어 트리/DSL”로 만들고 해석기에서 실행 | 완전한 분리, 매우 테스트 친화적 | 성능 느림, 코드 복잡        | DSL, 복잡한 워크플로우, 테스트 중심 개발       |
| **Tagless Final** | 인터페이스(타입클래스)와 구현을 분리한 고성능 추상화    | 고성능, 유연함, 현대적      | 난해한 타입 에러, 고급 개념 필요 | 대규모 프로젝트, 고성능 요구, 다중 backend 필요 |

---

### 어떤 패턴을 선택해야 할까?

- 작은 프로젝트 / 실용성 중시 → **MTL**
- 로직을 명령어/DSL로 다뤄야 함 → **Free Monad**
- 대규모 / 고성능 / 추상화 수준 높게 유지 → **Tagless Final**

---

###  **1. MTL (Monad Transformer Library)**

#### ■ 개념

하스켈에서 **모나드 스택(ReaderT / StateT / ExceptT / IO 등)** 을 조합하여
“필요한 능력(capability)”을 타입클래스로 추상화하는 패턴입니다.

#### ■ 핵심 아이디어

* `MonadReader env m`
* `MonadState s m`
* `MonadError e m`
* `MonadIO m`

이런 **MTL 타입클래스 제약을 함수 서명에 넣어 효과(Effect)를 추상화**합니다.

#### ■ 예시

```haskell
foo :: (MonadReader Config m, MonadIO m) => m ()
foo = do
  cfg <- ask
  liftIO (print cfg)
```

#### ■ 장점

* 사용이 간단하고 학습 비용이 낮습니다.
* 기존 Haskell 생태계와 자연스럽게 맞습니다.
* 런타임 비용이 거의 없습니다.
* 테스트 시 ReaderT/StateT 같은 모나드를 갈아끼우기 쉬움.

#### ■ 단점

* 깊게 중첩된 모나드 스택은 디버깅이 어렵습니다.
* 스택의 구성이 바뀌면 코드가 깨질 위험이 있습니다.
* Type error 메시지가 길어질 수 있음.
* 모나드 스택에 종속되어 있고, 재사용성이 한정됩니다.

---

###  **2. Free Monad 패턴**

#### ■ 개념

**프로그램을 “추상 명령어의 리스트”처럼 표현**하고,
그 명령어를 **나중에 해석기(Interpreter)** 를 통해 실행시키는 구조입니다.

즉,

> “로직의 구조(What)”와 “효과의 실제 실행(How)”을 완전히 분리

#### ■ 핵심 아이디어

* DSL(도메인 명령어)을 Functor로 정의
* Free Monad로 lift
* Interpreter로 IO 등 실제 부수효과 구현

#### ■ 예시 (단순화)

```haskell
data ConsoleF x
  = PrintLn String x
  | ReadLn (String -> x)

type Console = Free ConsoleF

printLn :: String -> Console ()
printLn s = liftF (PrintLn s ())

interpret :: Console a -> IO a
interpret (Free (PrintLn s next)) = putStrLn s >> interpret next
interpret (Free (ReadLn f)) = getLine >>= interpret . f
interpret (Pure x) = return x
```

#### ■ 장점

* **로직을 완전히 순수하게 유지 가능**
* 여러 개의 해석기(테스트용, 실제 IO용 등)를 쉽게 교체 가능
* 프로그램을 “데이터”처럼 다룰 수 있어 분석·리팩터링이 용이
* 복잡한 트랜잭션, 순서 제어, mock 테스트에 매우 강력

#### ■ 단점

* **성능이 느릴 수 있음** (특히 깊은 Free 체인)
* 코드가 장황해지고 Functor/MFunctor/MonadFree 등 구조가 복잡해짐
* 초보자에게 난해한 추상화

---

### **3. Tagless Final**

#### ■ 개념

**효과를 타입클래스의 “메서드 집합 인터페이스”로 추상화하고,
구현(인터프리터)을 인스턴스로 분리**하는 방식.

Free Monad보다 훨씬 타입 안전하고, 성능 문제가 거의 없음.

#### ■ 핵심 아이디어

```haskell
class Monad m => Console m where
  printLn :: String -> m ()
  readLn  :: m String
```

이 인터페이스만 있으면 **로직은 모나드가 무엇인지 모른 채 작성 가능**.

#### ■ 예시

```haskell
program :: Console m => m ()
program = do
  printLn "Enter:"
  name <- readLn
  printLn ("Hello, " ++ name)
```

인터프리터는 별도로 구현:

```haskell
instance Console IO where
  printLn = putStrLn
  readLn  = getLine
```

#### ■ 장점

* Free Monad보다 **성능이 매우 좋음** (zero cost abstraction에 가까움)
* 로직을 완전히 인터페이스로 분리
* 여러 인터프리터를 쉽게 사용 가능
* 타입 안전성이 매우 높고, 복잡한 도메인 모델에 적합
* 대규모 프로젝트에서 가장 현대적인 방식

#### ■ 단점

* 타입 추론이 어렵거나 오류 메시지가 난해할 수 있음
* Free Monad처럼 "프로그램을 데이터로 다루는 방식"은 어려움
* 고급 타입기술(XML-like 타입클래스 구조 등)이 필요한 경우가 많음

---

## 위 3가지 패턴으로 동일한 Todo 프로그램을 각각 구현한 비교 코드

세 가지 아키텍처 패턴(MTL / Free Monad / Tagless Final)로 동일한 Todo 프로그램을 구현한 비교 코드를 **Haskell Todo Arch Patterns**라는 파일로 만들어 두었습니다.

구성은 다음과 같습니다:

### 포함된 내용

* 공통 도메인 타입 (`Todo`, `TodoId`, `TodoList`)
* **MTL 버전**

  * `MonadTodoMTL` 타입클래스
  * `AppMTL` 모나드
  * `addTodoMTL`, `completeTodoMTL`, `listTodoMTL`
* **Free Monad 버전**

  * `TodoF` Functor
  * `TodoFree` DSL
  * `runFreeIO` 인터프리터
* **Tagless Final 버전**

  * `TodoTagless` 인터페이스
  * `AppTF` 인터프리터

### 예시 코드

```haskell
-- 비교 코드: 동일한 TODO 프로그램을 MTL, Free Monad, Tagless Final 3가지 패턴으로 구현

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

------------------------------------------------------------
-- 공통 도메인 타입
------------------------------------------------------------

newtype TodoId = TodoId Int deriving (Show, Eq)
data Todo = Todo { todoId :: TodoId, todoTitle :: String, todoDone :: Bool }
  deriving (Show, Eq)

type TodoList = [Todo]

------------------------------------------------------------
-- 1) MTL 기반 구현
------------------------------------------------------------

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class

class Monad m => MonadTodoMTL m where
  loadTodos :: m TodoList
  saveTodos :: TodoList -> m ()

newtype AppMTL a = AppMTL { runAppMTL :: StateT TodoList IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TodoList)

instance MonadTodoMTL AppMTL where
  loadTodos = get
  saveTodos = put

addTodoMTL :: MonadTodoMTL m => String -> m ()
addTodoMTL title = do
  todos <- loadTodos
  let newId = TodoId (length todos + 1)
  saveTodos (todos ++ [Todo newId title False])

completeTodoMTL :: MonadTodoMTL m => TodoId -> m ()
completeTodoMTL tid = do
  ts <- loadTodos
  let ts' = map (\t -> if todoId t == tid then t { todoDone = True } else t) ts
  saveTodos ts'

listTodoMTL :: MonadTodoMTL m => m TodoList
listTodoMTL = loadTodos

------------------------------------------------------------
-- 2) Free Monad 기반 구현
------------------------------------------------------------

import Control.Monad.Free

data TodoF next
  = LoadF ([Todo] -> next)
  | SaveF TodoList next
  deriving Functor

type TodoFree = Free TodoF

loadF :: TodoFree TodoList\ loadF = liftF (LoadF id)

saveF :: TodoList -> TodoFree ()
saveF ts = liftF (SaveF ts ())

addTodoFree :: String -> TodoFree ()
addTodoFree title = do
  ts <- loadF
  let newId = TodoId (length ts + 1)
  saveF (ts ++ [Todo newId title False])

completeTodoFree :: TodoId -> TodoFree ()
completeTodoFree tid = do
  ts <- loadF
  let ts' = map (\t -> if todoId t == tid then t { todoDone = True } else t) ts
  saveF ts'

listTodoFree :: TodoFree TodoList
listTodoFree = loadF

-- Free Monad 인터프리터 (메모리 IO)
runFreeIO :: IORef TodoList -> TodoFree a -> IO a
runFreeIO ref (Free (LoadF next)) = readIORef ref >>= runFreeIO ref . next
runFreeIO ref (Free (SaveF ts next)) = writeIORef ref ts >> runFreeIO ref next
runFreeIO _   (Pure x) = pure x

------------------------------------------------------------
-- 3) Tagless Final 기반 구현
------------------------------------------------------------

class Monad m => TodoTagless m where
  loadT :: m TodoList
  saveT :: TodoList -> m ()

addTodoTF :: TodoTagless m => String -> m ()
addTodoTF title = do
  ts <- loadT
  let newId = TodoId (length ts + 1)
  saveT (ts ++ [Todo newId title False])

completeTodoTF :: TodoTagless m => TodoId -> m ()
completeTodoTF tid = do
  ts <- loadT
  let ts' = map (\t -> if todoId t == tid then t { todoDone = True } else t) ts
  saveT ts'

listTodoTF :: TodoTagless m => m TodoList
listTodoTF = loadT

-- Tagless Final 인터프리터: IORef 사용
newtype AppTF a = AppTF { runAppTF :: ReaderT (IORef TodoList) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef TodoList))

instance TodoTagless AppTF where
  loadT = do ref <- ask; liftIO (readIORef ref)
  saveT ts = do ref <- ask; liftIO (writeIORef ref ts)

------------------------------------------------------------
-- END: 세 패턴 비교 코드
------------------------------------------------------------
```

## 3가지 패턴을 결합한 "현대 하스켈 아키텍처" 예시

요청하신 **MTL + Free Monad + Tagless Final** 을 조합한 “현대 하스켈 아키텍처” 예시를
**Modern Haskell Architecture** 파일에 생성해 두었습니다.

---

### 이 아키텍처가 현대적이라고 불리는 이유

#### ✔ Tagless Final

→ **도메인 로직(TodoStore)의 추상화 계층**

* IO, DB, 테스트 백엔드를 자유롭게 교체 가능
* 가장 타입 안전하고 고성능

#### ✔ Free Monad

→ **고수준 워크플로우 / 시나리오 / DSL 표현**

* 비즈니스 로직을 “스크립트처럼” 정의할 수 있음
* 여러 인터프리터 가능 (log-only, dry-run 등)

#### ✔ MTL (ReaderT AppEnv IO)

→ **애플리케이션 환경/리소스 관리**

* IORef / DB Pool / 설정 / 캐시 주입
* 부수효과를 안전하게 캡슐화

#### 전체 구조 요약

```
High-level Workflow (Free DSL)
     ↓
Business Logic (Tagless Final class)
     ↓
Interpreter (MTL / ReaderT + IORef, DB, etc.)
     ↓
Actual Effects
```

---

### 이 구조의 장점

| 기능         | 제공하는 레이어      | 설명                        |
| ---------- | ------------- | ------------------------- |
| 도메인 규칙     | Tagless Final | 타입 안정 + 컨포저블한 로직          |
| 워크플로우 DSL  | Free Monad    | 테스트/해석 가능성이 뛰어남           |
| 실제 부수효과    | MTL           | ReaderT 기반으로 깔끔하게 관리      |
| 교체 가능한 인프라 | TF + Free 조합  | IO → Mock → Logging 전환 가능 |

---

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- 현대 하스켈 아키텍처 예제
-- MTL + Free Monad + Tagless Final 조합 구조
-- 핵심 아이디어: 비즈니스 로직은 Tagless Final, 워크플로우/DSL은 Free, 
-- 환경/리소스는 MTL(ReaderT) 기반으로 구성

------------------------------------------------------------
-- 공통 도메인
------------------------------------------------------------

data Todo = Todo { todoId :: Int, todoTitle :: String, todoDone :: Bool }
  deriving Show

type TodoList = [Todo]

------------------------------------------------------------
-- 1) Tagless Final: 비즈니스 로직 인터페이스
------------------------------------------------------------

class Monad m => TodoStore m where
  loadTodos :: m TodoList
  saveTodos :: TodoList -> m ()

addTodo :: TodoStore m => String -> m ()
addTodo title = do
  ts <- loadTodos
  let new = Todo (length ts + 1) title False
  saveTodos (ts ++ [new])

markDone :: TodoStore m => Int -> m ()
markDone n = do
  ts <- loadTodos
  let ts' = map (\t -> if todoId t == n then t { todoDone = True } else t) ts
  saveTodos ts'

------------------------------------------------------------
-- 2) Free Monad: 고수준 워크플로우 DSL
------------------------------------------------------------

import Control.Monad.Free
import Data.Functor

-- DSL 정의

data WorkflowF next
  = AddTask String next
  | CompleteTask Int next
  | ShowAll ([Todo] -> next)
  deriving Functor

type Workflow = Free WorkflowF

-- DSL 명령어 리프팅
addTaskF :: String -> Workflow ()
addTaskF title = liftF (AddTask title ())

completeTaskF :: Int -> Workflow ()
completeTaskF n = liftF (CompleteTask n ())

showAllF :: Workflow TodoList
showAllF = liftF (ShowAll id)

------------------------------------------------------------
-- 3) MTL 기반 App 환경 + 인터프리터 조합
------------------------------------------------------------

import Control.Monad.Reader
import Data.IORef
import Control.Monad.IO.Class

newtype App a = App { runApp :: ReaderT (IORef TodoList) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef TodoList))

-- Tagless Final 인터프리터
instance TodoStore App where
  loadTodos = ask >>= liftIO . readIORef
  saveTodos ts = ask >>= \ref -> liftIO (writeIORef ref ts)

-- Free Workflow 인터프리터 → Tagless Final 호출
runWorkflow :: Workflow a -> App a
runWorkflow (Pure x) = return x
runWorkflow (Free (AddTask title next)) = addTodo title >> runWorkflow next
runWorkflow (Free (CompleteTask n next)) = markDone n >> runWorkflow next
runWorkflow (Free (ShowAll f)) = f <$> loadTodos >>= runWorkflow

------------------------------------------------------------
-- 4) 실제 실행 예시
------------------------------------------------------------

main :: IO ()
main = do
  ref <- newIORef []
  let app = do
        runWorkflow $ do
          addTaskF "Learn Haskell"
          addTaskF "Write modern architecture"
          completeTaskF 1
          todos <- showAllF
          liftIO (print todos)

  runReaderT (runApp app) ref
```
