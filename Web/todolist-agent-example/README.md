# Haskell TodoList — 에이전트 작업 준비 예제

명시적인 제품 명세, 아키텍처 규칙, 작업 파일, 인수 기준을 바탕으로 AI 코딩 에이전트가 작업할 수 있도록 Haskell/Stack 저장소를 구성하는 방법을 보여 주는 작은 TodoList REST API이다.

## 기술 스택

- Haskell / GHC 9.10.3
- Stack, Stackage LTS 24.58
- Servant / Warp
- `postgresql-simple`을 통한 PostgreSQL
- Hspec / hspec-wai

## 기능

- Todo 생성
- 생성 순서대로 Todo 목록 조회
- Todo 단건 조회
- Todo를 멱등적으로 완료
- Todo 삭제

버전 1에는 의도적으로 인증, 태그, 기한, 우선순위 또는 완료된 Todo의 재개방 기능이 없다.

## 저장소 구성

- `AGENTS.md` — 코딩 에이전트 규칙
- `docs/` — 제품, API, 아키텍처, 보안 및 인수 명세
- `tasks/` — 범위가 정해진 구현 작업 단위
- `src/Todo/Domain/` — 순수 도메인 모델
- `src/Todo/Application/` — 유스케이스 및 리포지터리 포트
- `src/Todo/Infrastructure/` — PostgreSQL 어댑터
- `src/Todo/Interface/HTTP/` — Servant API 및 핸들러
- `test/` — PostgreSQL이 필요 없는 단위 및 HTTP 테스트
- `integration/` — 선택 사항인 PostgreSQL 통합 검사
- `migrations/` — SQL 마이그레이션

## 사전 요구 사항

Stack과 PostgreSQL을 설치한다. 그런 다음 다음과 같이 데이터베이스를 생성한다.

```sh
createdb todo_list
psql todo_list < migrations/001_create_todos.sql
```

설정을 지정한다.

```sh
cp .env.example .env
export APP_PORT=8080
export DATABASE_URL='postgresql://localhost/todo_list'
```

애플리케이션은 환경 변수를 직접 읽는다. `.env`는 예제 파일이며 자동으로 로드되지 않는다.

## 빌드 및 테스트

```sh
stack build --test --no-run-tests
stack test
```

선택 사항인 DB 통합 검사를 실행한다.

```sh
export TEST_DATABASE_URL='postgresql://localhost/todo_list'
stack test todo-list:todo-list-integration
```

## 실행

```sh
stack run todo-list
```

그다음 다음을 실행한다.

```sh
curl -i \
  -H 'Content-Type: application/json' \
  -d '{"title":"Buy milk","description":"2 liters"}' \
  http://localhost:8080/api/v1/todos

curl -i http://localhost:8080/api/v1/todos
```

## 에이전트 작업 흐름

에이전트에는 한 번에 하나의 작업을 전달한다. 예시는 다음과 같다.

```text
먼저 AGENTS.md를 읽어라.
tasks/TODO-001-create-todo.md와 여기에서 참조하는 명세를 읽어라.
아직 코드를 수정하지 말고, 먼저 구현 계획을 작성하라.
```

명세가 권위 있는 기준이다. 에이전트는 명세 변경을 제안할 수 있지만, 구현에 맞추기 위해 제품 동작을 조용히 변경해서는 안 된다.

## 생성된 Cabal 메타데이터 참고 사항

`package.yaml`은 Hpack 소스이고 `todo-list.cabal`은 재현성과 검토를 위해 저장소에 포함된다. `package.yaml`을 수정하면 Hpack/Stack으로 Cabal 파일을 다시 생성하고 두 파일을 모두 커밋한다.
