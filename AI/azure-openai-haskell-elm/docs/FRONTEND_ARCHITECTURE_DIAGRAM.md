# Elm Frontend Architecture Diagram

## Clean Architecture 계층 구조

```
┌─────────────────────────────────────────────────────────────────┐
│                         Main.elm                                │
│                   (Application Entry Point)                     │
│                                                                 │
│  - Program initialization                                       │
│  - Elm Architecture (Model, Update, View)                       │
│  - Layer composition                                            │
└────────────────────────┬────────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ↓               ↓               ↓
┌────────────────┐ ┌────────────┐ ┌──────────────────┐
│  Presentation  │ │ Application│ │ Infrastructure   │
│     Layer      │ │    Layer   │ │      Layer       │
└────────────────┘ └────────────┘ └──────────────────┘
         │               │               │
         │               ↓               │
         │      ┌─────────────────┐      │
         │      │   Domain Layer  │      │
         │      │  (Core Business)│      │
         │      └─────────────────┘      │
         │               ↑               │
         └───────────────┴───────────────┘
```

## 상세 계층 구조

```
src/
│
├── Main.elm                          # 진입점
│   ├── Model (ChatState + apiUrl)
│   ├── Msg (UpdateInput, SendMessage, etc.)
│   ├── init, update, view
│   └── subscriptions
│
├── Domain/                           # 도메인 계층
│   ├── Message.elm
│   │   ├── Role (User | Assistant)
│   │   ├── Message { role, content }
│   │   ├── create : Role -> String -> Message
│   │   └── roleToString : Role -> String
│   │
│   └── ChatService.elm
│       ├── ChatService { sendMessage }
│       └── sendMessage : ChatService -> String -> List Message -> Cmd msg
│
├── Application/                      # 애플리케이션 계층
│   ├── ChatState.elm
│   │   ├── ChatState { messages, input, isLoading, error }
│   │   ├── init : ChatState
│   │   ├── addMessage : Message -> ChatState -> ChatState
│   │   ├── clearMessages : ChatState -> ChatState
│   │   ├── setLoading : Bool -> ChatState -> ChatState
│   │   ├── setError : String -> ChatState -> ChatState
│   │   ├── clearError : ChatState -> ChatState
│   │   ├── updateInput : String -> ChatState -> ChatState
│   │   └── getters (getMessages, getInput, isLoading, getError)
│   │
│   └── ChatUseCase.elm
│       ├── sendUserMessage : String -> ChatState -> (ChatState, Maybe Message)
│       ├── handleResponse : Result String String -> ChatState -> ChatState
│       ├── clearChat : ChatState -> ChatState
│       └── updateInput : String -> ChatState -> ChatState
│
├── Infrastructure/                   # 인프라 계층
│   ├── Error.elm
│   │   └── httpErrorToString : Http.Error -> String
│   │
│   └── Http/
│       ├── ChatApi.elm
│       │   ├── chatService : (Result String String -> msg) -> ChatService msg
│       │   └── sendChatRequest : (Result String String -> msg) -> String -> List Message -> Cmd msg
│       │
│       └── Decoder.elm
│           ├── chatResponseDecoder : Decoder String
│           ├── encodeChatRequest : List Message -> Value
│           └── encodeMessage : Message -> Value
│
└── Presentation/                     # 프레젠테이션 계층
    ├── View.elm
    │   ├── ViewConfig { onUpdateInput, onSendMessage, onClearChat, onKeyDown }
    │   └── view : ViewConfig msg -> ChatState -> Html msg
    │
    └── Components/
        ├── Header.elm
        │   └── view : Bool -> msg -> Html msg
        │
        ├── MessageList.elm
        │   ├── view : List Message -> Bool -> Maybe String -> Html msg
        │   ├── viewWelcome : Html msg
        │   ├── viewMessage : Message -> Html msg
        │   ├── viewLoadingMessage : Html msg
        │   └── viewError : String -> Html msg
        │
        └── InputArea.elm
            ├── view : String -> Bool -> (String -> msg) -> (Int -> msg) -> msg -> Html msg
            ├── onKeyDown : (Int -> msg) -> Attribute msg
            └── keyCode : Decoder Int
```

## 의존성 그래프

```
Main.elm
  ├─→ Application.ChatState
  ├─→ Application.ChatUseCase
  │     └─→ Application.ChatState
  │           └─→ Domain.Message
  ├─→ Domain.ChatService
  │     └─→ Domain.Message
  ├─→ Infrastructure.Http.ChatApi
  │     ├─→ Domain.ChatService
  │     ├─→ Domain.Message
  │     ├─→ Infrastructure.Error
  │     └─→ Infrastructure.Http.Decoder
  │           └─→ Domain.Message
  └─→ Presentation.View
        ├─→ Application.ChatState
        └─→ Presentation.Components.*
              └─→ Domain.Message
```

## 데이터 흐름 (User Input → Server Response)

```
┌─────────────┐
│    User     │
│   Action    │
└──────┬──────┘
       │
       ↓
┌─────────────────────────────────────────┐
│         Presentation Layer              │
│  InputArea.view                         │
│    - Capture user input                 │
│    - Emit Msg (UpdateInput, SendMessage)│
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│            Main.elm                     │
│  update : Msg -> Model -> (Model, Cmd)  │
│    - Pattern match on Msg               │
│    - Delegate to UseCase                │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│        Application Layer                │
│  ChatUseCase.sendUserMessage            │
│    - Validate input                     │
│    - Create Message entity              │
│    - Update ChatState                   │
│    - Return (newState, maybeMessage)    │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│         Domain Layer                    │
│  Message.create                         │
│    - Create Message entity              │
│    - Pure business logic                │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│            Main.elm                     │
│  - Check if message was created         │
│  - Call ChatService.sendMessage         │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│      Infrastructure Layer               │
│  ChatApi.sendChatRequest                │
│    - Encode messages to JSON            │
│    - Send HTTP POST request             │
│    - Decode response                    │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│         Backend API                     │
│  POST /api/chat                         │
│    - Process request                    │
│    - Return response                    │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│      Infrastructure Layer               │
│  Decoder.chatResponseDecoder            │
│    - Decode JSON response               │
│    - Convert to domain type             │
│    - Handle errors                      │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│            Main.elm                     │
│  update : GotResponse result            │
│    - Delegate to UseCase                │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│        Application Layer                │
│  ChatUseCase.handleResponse             │
│    - Pattern match on Result            │
│    - Create assistant Message           │
│    - Update ChatState                   │
│    - Handle errors                      │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────────────────────────────────┐
│         Presentation Layer              │
│  View.view                              │
│    - Render updated state               │
│    - Display new message                │
│    - Update UI                          │
└──────┬──────────────────────────────────┘
       │
       ↓
┌─────────────┐
│     UI      │
│   Updated   │
└─────────────┘
```

## 계층별 책임

### Domain Layer (핵심 비즈니스 로직)
- ✅ 비즈니스 엔티티 정의
- ✅ 비즈니스 규칙 구현
- ❌ 외부 의존성 없음
- ❌ UI 관련 코드 없음
- ❌ 데이터베이스/API 코드 없음

### Application Layer (유스케이스)
- ✅ 비즈니스 로직 조율
- ✅ 상태 관리
- ✅ Domain 계층 사용
- ❌ UI 렌더링 없음
- ❌ HTTP 요청 없음

### Infrastructure Layer (외부 시스템)
- ✅ HTTP 통신
- ✅ JSON 인코딩/디코딩
- ✅ 에러 처리
- ✅ Domain 인터페이스 구현
- ❌ 비즈니스 로직 없음

### Presentation Layer (UI)
- ✅ UI 렌더링
- ✅ 사용자 상호작용
- ✅ 컴포넌트 조합
- ❌ 비즈니스 로직 없음
- ❌ HTTP 요청 없음

## 테스트 전략

```
Domain Layer
  └─→ Unit Tests (순수 함수)
        - Message.create
        - Message.roleToString

Application Layer
  └─→ Unit Tests (상태 변경)
        - ChatState operations
        - ChatUseCase logic

Infrastructure Layer
  └─→ Integration Tests (HTTP)
        - Mock HTTP responses
        - Test encoders/decoders

Presentation Layer
  └─→ Component Tests (UI)
        - Test view functions
        - Test event handlers

Main.elm
  └─→ End-to-End Tests
        - Full user flow
        - Integration of all layers
```

## 확장 시나리오

### 새로운 기능 추가: 메시지 편집

1. **Domain**: `Message.elm`에 `id` 필드 추가
2. **Application**: `ChatUseCase.elm`에 `editMessage` 함수 추가
3. **Infrastructure**: 필요시 API 엔드포인트 추가
4. **Presentation**: `MessageList.elm`에 편집 버튼 추가
5. **Main**: `EditMessage` Msg 추가

### 새로운 UI 컴포넌트 추가: 사이드바

1. **Presentation**: `Components/Sidebar.elm` 생성
2. **Presentation**: `View.elm`에서 사이드바 조합
3. **Main**: 필요시 새로운 Msg 추가

### 새로운 API 엔드포인트 추가: 파일 업로드

1. **Domain**: `File.elm` 엔티티 추가
2. **Domain**: `FileService.elm` 인터페이스 추가
3. **Infrastructure**: `Http/FileApi.elm` 구현
4. **Application**: `FileUseCase.elm` 추가
5. **Presentation**: `Components/FileUpload.elm` 추가
6. **Main**: 파일 관련 Msg 추가

## 결론

이 아키텍처는:
- 명확한 계층 분리
- 단방향 의존성
- 테스트 용이성
- 확장 가능성
- 유지보수 편의성

을 제공합니다.
