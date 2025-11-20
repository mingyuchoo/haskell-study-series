# Idiomatic Haskell Programming Style

## 하스켈 프로젝트 구현 시 가장 하스켈다운 구현 순서

하스켈로 프로젝트를 구현하실 때는 **“순차적으로 기능을 쌓아가는 방식”보다는 “문제를 수학적으로 모델링하고, 불변식을 정하고, 타입을 먼저 설계한 뒤 구현을 붙여 나가는 방식”**이 더 자연스럽고 하스켈 패러다임에 맞습니다.
아래에서는 **하스켈다운 구현 순서**를 단계별로 정리해 드리겠습니다.

| 단계 | 내용                         |
| -- | -------------------------- |
| 1  | 도메인 모델링 & 타입 정의        |
| 2  | 불변식·제약 조건 명시 (스마트 생성자) |
| 3  | 순수한 핵심 로직 구현           |
| 4  | 효과(IO) 레이어 따로 구현       |
| 5  | 타입클래스/모나드 인터페이스로 추상화   |
| 6  | 조합하여 Main 구현 (가장 마지막)  |

---

### 1. 도메인 모델링 → 타입 정의가 먼저

하스켈에서는 **타입이 곧 설계**입니다.
프로그램이 다루는 대상(도메인)이 무엇인지 먼저 정하고 이를 타입으로 표현합니다.

예)

```haskell
type UserId = Int

data User = User
  { userId   :: UserId
  , userName :: Text
  , age      :: Int
  }
```

**이유:**

* 타입을 먼저 정의하면 이후 로직이 자연스럽게 타입 체커에 의해 안전하게 “가이드”됩니다.
* 하스켈은 “타입을 적으면 구현이 반쯤 끝난 것”이라는 말이 있을 정도로 타입 중심 언어입니다.

---

### 2. 불변식(Invariant)·제약 조건 명시

타입을 정의했으면 해당 타입이 지켜야 할 조건들을 명확히 합니다.

예)

* `age >= 0`
* `UserId`는 고유
* 리스트는 항상 정렬된 상태를 유지해야 한다 등

이 불변식은 **스마트 생성자(smart constructor)** 로 표현하는 것이 일반적입니다.

```haskell
mkAge :: Int -> Maybe Age
mkAge n
  | n >= 0    = Just (Age n)
  | otherwise = Nothing
```

---

### 3. 순수한 핵심 로직(Pure Core) 먼저

하스켈의 핵심 미덕은 **순수 함수(pure function)** 입니다.
가능한 한 “입출력 없는 순수 로직”을 먼저 작성하세요.

예)

* 정렬
* 검증
* 도메인 계산
* 상태 변환(State transition) 등

이 단계에서는 IO가 절대 등장하지 않습니다.

---

### 4. I/O 또는 모나드 인터페이스 분리

순수 로직이 완성되면 그 로직을 **I/O 계층과 분리**해서 연결합니다.

예)

* 파일 읽기/쓰기
* DB 접근
* HTTP 요청
* 무작위(random) 생성 등

핵심은 **로직과 부수효과를 절대 섞지 않는 것**입니다.

하스켈다운 패턴 예:

* 인터페이스를 typeclass 로 추상화
* 실제 I/O 는 `ReaderT` 또는 `ReaderT + IO` 조합으로 구현

---

### 5. 타입클래스 또는 인터페이스 레이어 생성 (옵션)

규모가 있는 프로젝트라면 다음과 같은 패턴을 사용합니다.

* `MonadLogger m => ...`
* `MonadDB m => ...`
* `MonadRandom m => ...`

이 레이어가 있으면 순수 코드 테스트가 매우 쉬워지고, 의존성 주입처럼 동작합니다.

---

### 6. 조합하고 실행하는 Main 작성

초기에는 `Main`을 작성하고 싶어도 참아야 합니다.
하스켈다운 방식에서는 **Main은 가장 마지막에 쓰는 것이 일반적입니다.**

Main에서는 단순히 다음만 합니다:

1. 환경 읽기
2. 필요한 IO 리소스 초기화
3. 위에서 만든 순수 로직 + 모나드 인터페이스를 조합해 실행

---

### 🔁 반복: 타입 → 불변식 → 순수로직 → 인터페이스 계층

이 패턴을 계속 반복하면서 확장합니다.

---

## 단계별 실제 예제

### 간단한 단계별 Todo CLI 예제

```haskell
{-# LANGUAGE OverloadedStrings #-}
-- 간단한 Todo CLI 예제 (단계별)
-- 1) 도메인 모델링: 타입 정의
-- 2) 불변식: 스마트 생성자
-- 3) 순수한 핵심 로직: add/complete/list
-- 4) 효과 레이어: MonadTodo 타입클래스 + IO 구현
-- 5) Main: 조합해서 실행

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Environment (getArgs)
import Text.Printf (printf)

-- =========================
-- 1) 도메인 모델링 & 타입 정의
-- =========================

newtype TodoId = TodoId Int
  deriving (Eq, Show)

newtype Title = Title Text
  deriving (Eq, Show)

data Todo = Todo
  { todoId    :: TodoId
  , todoTitle :: Title
  , todoDone  :: Bool
  }
  deriving (Eq, Show)

newtype TodoList = TodoList [Todo]
  deriving (Eq, Show)

-- =========================
-- 2) 불변식 (스마트 생성자)
-- =========================

-- Title은 빈 문자열일 수 없다
mkTitle :: Text -> Maybe Title
mkTitle t
  | T.null (T.strip t) = Nothing
  | T.length (T.strip t) > 200 = Nothing -- 임의의 길이 제약
  | otherwise = Just (Title (T.strip t))

-- TodoId 생성 (간단히 숫자 증가)
nextId :: TodoList -> TodoId
nextId (TodoList []) = TodoId 1
nextId (TodoList ts)  = let (
    TodoId n) = todoId (last ts) in TodoId (n + 1)

-- =========================
-- 3) 순수한 핵심 로직
-- =========================

addTodoPure :: Title -> TodoList -> TodoList
addTodoPure title tl@(TodoList ts) =
  let tid = nextId tl
      t = Todo tid title False
  in TodoList (ts ++ [t])

completeTodoPure :: TodoId -> TodoList -> Maybe TodoList
completeTodoPure tid (TodoList ts) =
  if any ((== tid) . todoId) ts
    then let ts' = map mark ts in Just (TodoList ts')
    else Nothing
  where
    mark t
      | todoId t == tid = t { todoDone = True }
      | otherwise = t

listTodosPure :: TodoList -> [Todo]
listTodosPure (TodoList ts) = ts

-- =========================
-- 4) 효과 레이어: 타입클래스와 IO 구현
-- =========================

-- 의존성을 추상화: Todo 저장소 인터페이스
class Monad m => MonadTodo m where
  loadTodos :: m TodoList
  saveTodos :: TodoList -> m ()

-- 간단한 IO 구현: 파일 대신 메모리 IORef 사용
-- AppEnv에는 IORef TodoList가 들어있다
newtype AppEnv = AppEnv { appStore :: IORef TodoList }

newtype AppM a = AppM { runAppM :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

instance MonadTodo AppM where
  loadTodos = do
    ref <- asks appStore
    liftIO (readIORef ref)
  saveTodos tl = do
    ref <- asks appStore
    liftIO (writeIORef ref tl)

-- Helper to run AppM
runWithEmptyStore :: AppM a -> IO a
runWithEmptyStore app = do
  ref <- newIORef (TodoList [])
  let env = AppEnv ref
  runReaderT (runAppM app) env

-- 한 단계 위의 순수 로직을 사용해 실제 동작 구현
addTodo :: Title -> AppM ()
addTodo title = do
  tl <- loadTodos
  let tl' = addTodoPure title tl
  saveTodos tl'

completeTodo :: TodoId -> AppM Bool
completeTodo tid = do
  tl <- loadTodos
  case completeTodoPure tid tl of
    Just tl' -> saveTodos tl' >> return True
    Nothing  -> return False

getTodos :: AppM [Todo]
getTodos = do
  tl <- loadTodos
  return (listTodosPure tl)

-- =========================
-- 5) Main: 조합해서 CLI 실행
-- =========================

printTodo :: Todo -> IO ()
printTodo (Todo (TodoId i) (Title t) done) =
  putStrLn $ printf "%d. [%c] %s" i (if done then 'x' else ' ') (T.unpack t)

usage :: IO ()
usage = putStrLn "Usage: todo add <title> | todo list | todo done <id>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("add":xs) ->
      let raw = T.pack (unwords xs)
      in case mkTitle raw of
           Nothing -> putStrLn "Invalid title (empty or too long)"
           Just title -> runWithEmptyStore (addTodo title) >> putStrLn "Added."

    ["list"] -> do
      todos <- runWithEmptyStore getTodos
      mapM_ printTodo todos

    ("done":[iStr]) ->
      case reads iStr of
        [(n,"")] -> do
          ok <- runWithEmptyStore (completeTodo (TodoId n))
          putStrLn $ if ok then "Marked done." else "No such id."
        _ -> putStrLn "Invalid id"

    _ -> usage

-- 참고: 이 예제는 교육용으로 단순화되어 있습니다.
-- 실제 프로젝트에서는 파일/DB 저장, 에러 처리, 커맨드 파싱 라이브러리(optparse-applicative) 사용,
-- 영속성 계층(파일이나 DB) 구현 등을 추가하시면 됩니다.
```

## 하스켈의 아키텍처 패턴(MTL, Free Monad, Tagless Final)의 차이도 비교

아래에서는 **하스켈에서 자주 사용되는 3가지 아키텍처 패턴(MTL, Free Monad, Tagless Final)** 을
**개념 → 예시 → 장단점 → 서로의 차이** 순서로 명확하게 비교해 드리겠습니다.
존댓말 유지해서 설명드릴게요.

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

### 3가지 패턴 요약 비교

| 패턴                | 핵심 개념                            | 장점                 | 단점                  | 적합한 경우                          |
| ----------------- | -------------------------------- | ------------------ | ------------------- | ------------------------------- |
| **MTL**           | 모나드 스택 + 타입클래스 능력                | 단순, 빠름, 실용적        | 스택 의존성, 타입 오류 복잡    | 중소 규모 프로젝트, 일반적인 Haskell        |
| **Free Monad**    | 프로그램을 “명령어 트리/DSL”로 만들고 해석기에서 실행 | 완전한 분리, 매우 테스트 친화적 | 성능 느림, 코드 복잡        | DSL, 복잡한 워크플로우, 테스트 중심 개발       |
| **Tagless Final** | 인터페이스(타입클래스)와 구현을 분리한 고성능 추상화    | 고성능, 유연함, 현대적      | 난해한 타입 에러, 고급 개념 필요 | 대규모 프로젝트, 고성능 요구, 다중 backend 필요 |

---

### 어떤 패턴을 선택해야 할까?

#### **작은 프로젝트 / 실용성 중시**

→ **MTL**

#### **로직을 명령어/DSL로 다뤄야 함**

→ **Free Monad**

#### **대규모 / 고성능 / 추상화 수준 높게 유지**

→ **Tagless Final**

---

## 위 3가지 패턴으로 동일한 Todo 프로그램을 각각 구현한 비교 코드

## 세 패턴의 실제 프로젝트 구조 예시

## 3가지 패턴을 결합한 "현대 하스켈 아키텍처" 예시

## 초보자에게 실전적으로 가장 좋은 접근 방식 추천
