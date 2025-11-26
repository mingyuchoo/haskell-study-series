# TUI Todo 애플리케이션 아키텍처

## 개요

TUI Todo는 Haskell로 작성된 터미널 기반 할일 관리 애플리케이션입니다. MTL (Monad Transformer Library) 패턴을 따르며, 순수 함수와 효과를 명확히 분리한 구조를 가지고 있습니다.

## 프로젝트 구조

```
src/
├── App.hs                    # MTL 기반 애플리케이션 로직 (효과)
├── Config.hs                 # 설정 파일 로딩 (효과)
├── DB.hs                     # 데이터베이스 작업 (효과)
├── I18n.hs                   # 다국어 지원 (순수 + 효과)
├── Lib.hs                    # 애플리케이션 진입점
└── UI/
    ├── Types.hs              # 데이터 타입 정의 (순수)
    ├── Attributes.hs         # UI 스타일 정의 (순수)
    ├── Draw.hs               # UI 렌더링 (순수)
    └── Events.hs             # 이벤트 처리 (효과)
```

## 모듈별 역할

### 순수(Pure) 모듈

#### UI/Types.hs
핵심 데이터 타입과 렌즈 정의:
- `Mode`: 애플리케이션 모드 (ViewMode, InputMode, EditMode)
- `Name`: 위젯 리소스 이름
- `FocusedField`: 포커스된 필드 추적
- `Todo`: UI Todo 항목 표현
- `AppState`: 애플리케이션 상태
- `fromTodoRow`: DB 행을 UI Todo로 변환하는 순수 함수

#### UI/Attributes.hs
UI 속성 및 스타일 정의:
- `theMap`: Vty 속성 맵
- 색상, 스타일 설정

#### UI/Draw.hs
UI 렌더링 로직 (순수 함수):
- `drawUI`: 메인 UI 그리기
- `drawHeader`: 헤더 렌더링
- `drawTodoList`: Todo 리스트 렌더링
- `drawTodo`: 개별 Todo 항목 렌더링
- `drawDetailView`: 상세 뷰 렌더링
- `drawHelp`: 도움말 렌더링

#### I18n.hs (부분)
다국어 메시지 데이터 타입:
- `Language`: 지원 언어 (English, Korean)
- `I18nMessages`: 다국어 메시지 구조
- `defaultMessages`: 기본 영어 메시지

### 효과(Effect) 모듈

#### App.hs
MTL 기반 애플리케이션 로직:
- `AppEnv`: 애플리케이션 환경 (Connection, I18nMessages)
- `AppM`: MTL 모나드 스택
- `MonadApp`: 애플리케이션 타입클래스
- Todo CRUD 작업: `loadTodos`, `createTodo`, `updateTodo`, `deleteTodo`, `toggleTodo`

#### DB.hs
SQLite 데이터베이스 작업:
- `TodoRow`: 데이터베이스 Todo 표현
- `initDB`, `initDBWithMessages`: 데이터베이스 초기화
- CRUD 함수들: `createTodo`, `getAllTodos`, `updateTodo`, `deleteTodo`, `toggleTodoComplete`

#### Config.hs
설정 파일 로딩:
- `KeyBindings`: 키바인딩 설정
- `loadKeyBindings`: YAML 파일에서 키바인딩 로드
- `matchesKey`: 키 매칭 (순수)
- `keyToString`: 키 변환 (순수)

#### I18n.hs (부분)
다국어 파일 로딩:
- `loadMessages`: YAML 파일에서 메시지 로드 (IO)

#### UI/Events.hs
이벤트 처리 로직:
- `handleEvent`: 메인 이벤트 핸들러
- 모드별 이벤트 처리: `handleViewMode`, `handleInputMode`, `handleEditMode`
- Todo 작업: 생성, 수정, 삭제, 토글
- 에디터 관리 함수들

#### Lib.hs
애플리케이션 진입점:
- `app`: Brick 애플리케이션 정의
- 모든 모듈 통합

## MTL 아키텍처

### 환경 분리
```haskell
data AppEnv = AppEnv 
  { envConnection :: !Connection
  , envMessages   :: !I18n.I18nMessages
  }
```

### 타입클래스 기반 추상화
```haskell
class (MonadIO m, MonadReader AppEnv m) => MonadApp m where
    getConnection :: m Connection
    getMessages :: m I18n.I18nMessages
```

### 모나드 스택
```haskell
newtype AppM a = AppM { unAppM :: ReaderT AppEnv IO a }
```

## 데이터 흐름

1. **초기화**: `Lib.hs`에서 데이터베이스, 설정, 다국어 메시지 로드
2. **상태 관리**: `AppState`에 UI 상태 저장
3. **이벤트 처리**: `UI/Events.hs`에서 사용자 입력 처리
4. **데이터베이스 작업**: `App.hs`를 통해 `DB.hs` 호출
5. **UI 렌더링**: `UI/Draw.hs`에서 순수 함수로 UI 생성

## 설계 원칙

1. **순수 함수와 효과 분리**: UI 렌더링은 순수, 데이터베이스 작업은 효과
2. **타입 안전성**: 강타입 시스템 활용
3. **모듈화**: 역할별로 모듈 분리
4. **테스트 가능성**: MTL 패턴으로 의존성 주입
5. **확장 가능성**: 새 기능 추가 시 기존 코드 최소 변경

## 확장 가이드

### 새로운 UI 컴포넌트 추가
`UI/` 디렉토리에 새 모듈 추가

### 새로운 이벤트 타입 추가
`UI/Events.hs`에 핸들러 추가

### 새로운 데이터베이스 작업 추가
1. `DB.hs`에 함수 추가
2. `App.hs`에 래퍼 함수 추가
3. `UI/Events.hs`에서 호출

### 새로운 언어 추가
1. `I18n.hs`의 `Language` 타입에 추가
2. `config/messages-XX.yaml` 파일 생성
3. `loadMessages` 함수에 경로 추가
