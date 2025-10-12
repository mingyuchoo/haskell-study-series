# Elm Frontend Clean Architecture 구현 가이드

## 개요

이 문서는 Elm 프론트엔드를 Clean Architecture 패턴으로 리팩토링한 내용을 설명합니다.

## Clean Architecture란?

Clean Architecture는 Robert C. Martin(Uncle Bob)이 제안한 소프트웨어 아키텍처 패턴으로, 다음과 같은 원칙을 따릅니다:

1. **독립성**: 프레임워크, UI, 데이터베이스, 외부 에이전시로부터 독립적
2. **테스트 가능성**: 비즈니스 규칙을 외부 요소 없이 테스트 가능
3. **UI 독립성**: UI를 쉽게 변경 가능
4. **데이터베이스 독립성**: 비즈니스 규칙이 데이터베이스에 바인딩되지 않음
5. **외부 에이전시 독립성**: 비즈니스 규칙이 외부 세계에 대해 알지 못함

## 계층 구조

### 1. Domain Layer (도메인 계층)

**위치**: `src/Domain/`

**책임**:
- 비즈니스 엔티티 정의
- 비즈니스 규칙 구현
- 외부 의존성 없음

**파일**:
- `Message.elm`: 메시지 엔티티와 Role 타입
  ```elm
  type Role = User | Assistant
  
  type alias Message =
      { role : Role
      , content : String
      }
  ```

- `ChatService.elm`: 채팅 서비스 인터페이스 (포트)
  ```elm
  type alias ChatService msg =
      { sendMessage : String -> List Message -> Cmd msg
      }
  ```

**특징**:
- 순수 함수형
- 외부 의존성 없음
- 가장 안정적인 계층

### 2. Application Layer (애플리케이션 계층)

**위치**: `src/Application/`

**책임**:
- 유스케이스 구현
- 비즈니스 로직 조율
- 상태 관리

**파일**:
- `ChatState.elm`: 채팅 상태 관리
  ```elm
  type alias ChatState =
      { messages : List Message
      , input : String
      , isLoading : Bool
      , error : Maybe String
      }
  ```

- `ChatUseCase.elm`: 채팅 유스케이스
  - `sendUserMessage`: 사용자 메시지 전송
  - `handleResponse`: 서버 응답 처리
  - `clearChat`: 채팅 초기화
  - `updateInput`: 입력 업데이트

**특징**:
- Domain 계층만 의존
- 비즈니스 로직의 흐름 제어
- 상태 변경을 불변 방식으로 처리

### 3. Infrastructure Layer (인프라 계층)

**위치**: `src/Infrastructure/`

**책임**:
- 외부 시스템과의 통신
- HTTP 요청/응답 처리
- JSON 인코딩/디코딩
- 에러 처리

**파일**:
- `Http/ChatApi.elm`: HTTP API 클라이언트
  ```elm
  chatService : (Result String String -> msg) -> ChatService msg
  chatService toMsg =
      { sendMessage = sendChatRequest toMsg
      }
  ```

- `Http/Decoder.elm`: JSON 인코더/디코더
  - `chatResponseDecoder`: 응답 디코더
  - `encodeChatRequest`: 요청 인코더
  - `encodeMessage`: 메시지 인코더

- `Error.elm`: HTTP 에러 처리
  ```elm
  httpErrorToString : Http.Error -> String
  ```

**특징**:
- Domain 계층의 인터페이스 구현
- 외부 라이브러리 사용 (elm/http, elm/json)
- 에러를 도메인 타입으로 변환

### 4. Presentation Layer (프레젠테이션 계층)

**위치**: `src/Presentation/`

**책임**:
- UI 렌더링
- 사용자 상호작용 처리
- 컴포넌트 조합

**파일**:
- `View.elm`: 메인 뷰 조합
  ```elm
  type alias ViewConfig msg =
      { onUpdateInput : String -> msg
      , onSendMessage : msg
      , onClearChat : msg
      , onKeyDown : Int -> msg
      }
  ```

- `Components/Header.elm`: 헤더 컴포넌트
- `Components/MessageList.elm`: 메시지 리스트 컴포넌트
- `Components/InputArea.elm`: 입력 영역 컴포넌트

**특징**:
- 재사용 가능한 컴포넌트
- 순수 뷰 함수 (부수 효과 없음)
- Application 계층의 상태를 렌더링

### 5. Main (진입점)

**위치**: `src/Main.elm`

**책임**:
- 모든 계층 조합
- Elm Architecture 적용
- 애플리케이션 초기화

**구조**:
```elm
type alias Model =
    { chatState : ChatState
    , apiUrl : String
    }

type Msg
    = UpdateInput String
    | SendMessage
    | GotResponse (Result String String)
    | ClearChat
    | KeyDown Int

update : Msg -> Model -> ( Model, Cmd Msg )
view : Model -> Html Msg
```

## 의존성 규칙

```
┌─────────────────────────────────────────┐
│           Presentation Layer            │
│  (View, Components)                     │
└─────────────────┬───────────────────────┘
                  │
                  ↓
┌─────────────────────────────────────────┐
│          Application Layer              │
│  (ChatState, ChatUseCase)               │
└─────────────────┬───────────────────────┘
                  │
                  ↓
┌─────────────────────────────────────────┐
│            Domain Layer                 │
│  (Message, ChatService)                 │
└─────────────────────────────────────────┘
                  ↑
                  │
┌─────────────────┴───────────────────────┐
│        Infrastructure Layer             │
│  (ChatApi, Decoder, Error)              │
└─────────────────────────────────────────┘
```

**규칙**:
1. 외부 계층은 내부 계층을 의존할 수 있음
2. 내부 계층은 외부 계층을 의존할 수 없음
3. Domain 계층은 어떤 계층도 의존하지 않음
4. Infrastructure는 Domain의 인터페이스를 구현

## 데이터 흐름

### 1. 사용자 입력 → 메시지 전송

```
User Input
    ↓
Presentation (InputArea)
    ↓ UpdateInput Msg
Main.update
    ↓
Application (ChatUseCase.updateInput)
    ↓
ChatState (updateInput)
    ↓
Updated Model
    ↓
Presentation (View)
```

### 2. 메시지 전송 → 서버 응답

```
User Click Send
    ↓
Presentation (InputArea)
    ↓ SendMessage Msg
Main.update
    ↓
Application (ChatUseCase.sendUserMessage)
    ↓
Domain (Message.create)
    ↓
Infrastructure (ChatApi.sendChatRequest)
    ↓ HTTP Request
Backend API
    ↓ HTTP Response
Infrastructure (Decoder.chatResponseDecoder)
    ↓ GotResponse Msg
Main.update
    ↓
Application (ChatUseCase.handleResponse)
    ↓
ChatState (addMessage)
    ↓
Updated Model
    ↓
Presentation (View)
```

## 장점

### 1. 테스트 용이성
- 각 계층을 독립적으로 테스트 가능
- Domain 계층은 순수 함수로만 구성되어 테스트가 쉬움
- Mock 객체를 사용하여 Infrastructure 계층 테스트 가능

### 2. 유지보수성
- 변경 사항이 특정 계층에 국한됨
- UI 변경 시 Presentation 계층만 수정
- API 변경 시 Infrastructure 계층만 수정
- 비즈니스 로직 변경 시 Application/Domain 계층만 수정

### 3. 확장성
- 새로운 기능 추가가 용이
- 새로운 컴포넌트를 Presentation 계층에 추가
- 새로운 유스케이스를 Application 계층에 추가
- 새로운 외부 서비스를 Infrastructure 계층에 추가

### 4. 재사용성
- Domain 계층의 엔티티와 규칙은 다른 프로젝트에서 재사용 가능
- Presentation 계층의 컴포넌트는 다른 페이지에서 재사용 가능
- Application 계층의 유스케이스는 다른 UI에서 재사용 가능

### 5. 명확한 책임 분리
- 각 계층이 명확한 책임을 가짐
- 코드의 위치를 쉽게 파악 가능
- 새로운 개발자가 프로젝트를 이해하기 쉬움

## Elm과 Clean Architecture

Elm은 Clean Architecture와 잘 맞는 언어입니다:

1. **순수 함수형**: 부수 효과가 없어 테스트가 쉬움
2. **강력한 타입 시스템**: 컴파일 타임에 에러 검출
3. **불변성**: 상태 변경이 명시적이고 추적 가능
4. **Cmd와 Sub**: 부수 효과를 명시적으로 처리
5. **모듈 시스템**: 계층 분리가 자연스러움

## 모범 사례

### 1. Domain 계층
- 외부 의존성을 절대 추가하지 않음
- 순수 함수만 사용
- 비즈니스 규칙을 명확하게 표현

### 2. Application 계층
- Domain 계층만 의존
- 상태 변경을 불변 방식으로 처리
- 유스케이스를 작고 명확하게 유지

### 3. Infrastructure 계층
- Domain의 인터페이스를 구현
- 외부 라이브러리 사용을 이 계층에 국한
- 에러를 도메인 타입으로 변환

### 4. Presentation 계층
- 순수 뷰 함수만 사용
- 컴포넌트를 작고 재사용 가능하게 유지
- 비즈니스 로직을 포함하지 않음

## 결론

Clean Architecture를 Elm에 적용하면:
- 코드의 구조가 명확해짐
- 테스트와 유지보수가 쉬워짐
- 확장성과 재사용성이 향상됨
- 팀 협업이 원활해짐

Elm의 강력한 타입 시스템과 순수 함수형 특성은 Clean Architecture의 원칙을 자연스럽게 따르게 만들어줍니다.
