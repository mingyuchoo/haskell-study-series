# Architecture

## 개요

브라우저의 Elm 단일 페이지 애플리케이션이 Haskell/WAI HTTP 서버의 JSON API를 호출한다. 서버는 Clean Architecture의 의존성 방향을 따라 인터페이스, 애플리케이션, 도메인, 인프라 계층을 분리한다.

```text
Elm Presentation → Elm Application/Infrastructure → HTTP JSON API
                                                   ↓
Interface.Http.TaskRoutes → Application.TaskService → Domain.Task
                                                   ↓
                         Infrastructure.InMemoryTaskRepository
```

## 서버 계층

| 계층 | 책임 | 주요 모듈 |
| --- | --- | --- |
| Interface | HTTP 라우팅, JSON 직렬화, 정적 파일 제공 | `Interface.Http.TaskRoutes` |
| Application | 유스케이스와 저장소 조합 | `Application.TaskService` |
| Domain | 상태 전이, 입력 검증, Outcome 조립 규칙 | `Domain.Task` |
| Infrastructure | 메모리 기반 저장·조회 | `Infrastructure.InMemoryTaskRepository` |
| Port | 저장소가 제공해야 하는 연산 정의 | `Application.Port.TaskRepository` |

## 클라이언트 계층

`web/src/Presentation/TaskBoard.elm`은 화면을 렌더링하고, `Application/TaskBoard.elm`은 상태와 메시지를 조정하며, `Infrastructure/TaskApi.elm`은 API 요청을 담당한다. 도메인 타입과 화면용 규칙은 `Domain/Task.elm`에 둔다.

## 배포 경계

- Warp 서버가 API와 `web/`의 정적 자산을 함께 제공한다.
- 영속 DB, 인증 제공자, 비동기 큐는 아직 연결되어 있지 않다.

## 출처

- `package.yaml`
- `src/`
- `web/src/`
