# Architecture

## 개요

브라우저의 Elm 단일 페이지 애플리케이션이 Haskell/WAI HTTP 서버의 JSON API를 호출한다. 서버는 Clean Architecture의 의존성 방향을 따라 인터페이스, 애플리케이션, 도메인, 인프라 계층을 분리한다.

```text
Elm Presentation → Elm Application/Infrastructure → HTTP JSON API
                                                   ↓
Interface.Http.TaskRoutes → Application.TaskService → Domain.Task
                                                   ↓
                         Application.Port.TaskRepository
                                                   ↓
                         Infrastructure.SQLiteTaskRepository

Interface.Http.TaskRoutes → Application.AuthService → Domain.User
                                                   ↓
                         Application.Port.UserRepository
                                                   ↓
                         Infrastructure.SQLiteUserRepository
```

## 서버 계층

| 계층 | 책임 | 주요 모듈 |
| --- | --- | --- |
| Interface | HTTP 라우팅, JSON 직렬화, 정적 파일 제공 | `Interface.Http.TaskRoutes` |
| Application | 유스케이스와 저장소 조합 | `Application.TaskService` |
| Domain | 상태 전이, 입력 검증, Outcome 조립 규칙 | `Domain.Task` |
| Infrastructure | SQLite 연결·마이그레이션과 영속 저장·조회 | `Infrastructure.SQLiteDatabase`, `Infrastructure.SQLiteTaskRepository`, `Infrastructure.SQLiteUserRepository` |
| Port | 저장소가 제공해야 하는 연산 정의 | `Application.Port.TaskRepository`, `Application.Port.UserRepository` |

## 클라이언트 계층

`web/src/Presentation/TaskBoard.elm`은 화면을 렌더링하고, `Application/TaskBoard.elm`은 상태와 메시지를 조정하며, `Infrastructure/TaskApi.elm`은 API 요청을 담당한다. 도메인 타입과 화면용 규칙은 `Domain/Task.elm`에 둔다.

## 배포 경계

- Warp 서버가 API와 `web/`의 정적 자산을 함께 제공한다.
- 기본 SQLite 파일은 프로세스 작업 디렉터리의 `general-service.sqlite3`이고 `DATABASE_PATH`로 바꿀 수 있다.
- `SEED_EXAMPLE_DATA`는 기본적으로 활성화되며, 리터럴 소문자 `false`일 때 예시 Task 시드를 비활성화한다.
- 서버 시작 시 `schema_migrations`를 기준으로 트랜잭션 단위 전진 마이그레이션을 적용한다. 서버가 지원하는 버전보다 새로운 DB는 열지 않는다.
- 프로세스는 하나의 공유 SQLite 연결을 `MVar`로 직렬화하며, `foreign_keys`, 5초 `busy_timeout`, WAL 저널, `synchronous=NORMAL`을 설정한다.
- 메모리 저장소 구현은 단위 테스트 대역으로 남지만 실행 서버는 SQLite 저장소를 사용한다.

## 운영 제약

- 이 구성은 단일 서버 인스턴스와 로컬 파일시스템을 전제로 한다. 여러 프로세스의 애플리케이션 연산을 하나의 트랜잭션 경계로 조정하지 않으므로 다중 서버 인스턴스나 공유 파일시스템 운영에는 적합하지 않다.
- 예시 Task 시드는 활성화된 경우 빈 Task 테이블에 최초 한 번만 실행되고 `app_metadata`에 이력을 남긴다. 운영 환경에서는 `SEED_EXAMPLE_DATA=false`로 비활성화한다. 기존 메모리 저장소의 실행 중 데이터는 자동 이관하지 않는다.
- 기존 운영 DB에 마이그레이션을 적용하기 전에는 일관된 백업과 복원 절차를 검증하고 별도 사람 승인을 받아야 한다.
- 외부 인증 제공자와 비동기 큐는 연결되어 있지 않다.

## 출처

- `package.yaml`
- `app/Main.hs`
- `src/Infrastructure/SQLiteDatabase.hs`
- `src/Infrastructure/SQLiteTaskRepository.hs`
- `src/Infrastructure/SQLiteUserRepository.hs`
- `web/src/`
