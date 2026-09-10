# Decisions

## ADR-001: Clean Architecture 계층 분리

- 상태: 채택
- 결정: HTTP, 애플리케이션 유스케이스, 도메인 규칙, 저장소 구현을 별도 모듈로 분리한다.
- 이유: 화면·전송 방식·저장소 구현의 변경이 도메인 규칙에 미치는 영향을 줄이기 위해서다.

## ADR-002: 메모리 저장소로 시작

- 상태: ADR-005로 대체됨
- 결정: 첫 버전은 `InMemoryTaskRepository`를 사용한다.
- 이유: 별도 인프라 없이 CRUD와 업무 흐름을 검증할 수 있다.
- 결과: 서버 재시작 시 데이터가 사라지며, 다중 인스턴스 운영에 적합하지 않다.

## ADR-003: 결과물 검토를 도메인 상태 전이로 강제

- 상태: 채택
- 결정: 제출자는 Task Owner와 일치해야 하며, Outcome Owner만 제출된 결과물을 승인·수정 요청할 수 있다.
- 이유: API 호출 경로와 무관하게 업무 규칙을 일관되게 보장한다.

## ADR-004: 신규 Task의 초안 고정

- 상태: 채택
- 결정: 생성 API와 UI는 신규 Task를 `Draft`로 시작시킨다.
- 이유: 생성 시점에 임의로 검토·승인 상태로 진입하는 것을 막는다.

## ADR-005: SQLite 영속 저장소 채택

- 상태: 채택
- 결정: 실행 서버의 Task, Outcome, 사용자와 세션 저장소를 SQLite로 교체한다. 기본 파일은 `general-service.sqlite3`이며 `DATABASE_PATH`로 경로를 재정의한다. 메모리 저장소는 단위 테스트 대역으로 유지한다.
- 연결: 프로세스당 하나의 연결을 `MVar`로 직렬화하고 외래 키, 5초 busy timeout, WAL, `synchronous=NORMAL`을 설정한다.
- 마이그레이션: 별도 도구 대신 `schema_migrations`와 코드에 정의한 버전별 SQL을 사용한다. 미적용 버전은 시작 시 트랜잭션으로 전진 적용하고 미래 버전 DB는 시작을 거부한다. 자동 롤백 마이그레이션은 제공하지 않는다.
- 초기 데이터: `SEED_EXAMPLE_DATA`가 활성화된 새 DB의 Task 테이블이 비어 있을 때 예시 Task를 최초 한 번만 추가하고 `app_metadata`에 처리 이력을 기록한다. 운영 환경은 리터럴 소문자 `false`로 시드를 비활성화한다. 기존 메모리 데이터는 자동 이관하지 않는다.
- 세션: 세션은 SQLite에 저장하고 생성 후 24시간에 만료한다. 만료된 토큰은 조회 시 삭제한다.
- 백업·복구: SQLite `.backup`으로 일관된 백업을 만들고 무결성을 확인한다. 복원은 서버를 중지하고 현재 DB를 별도로 백업한 뒤 수행한다. 기존 운영 DB에 마이그레이션·복원을 실제 적용하기 전에는 백업과 복원 리허설 및 별도 사람 승인이 필요하다.
- 결과: 정상적인 서버 재시작 뒤 데이터가 유지된다. 다만 현재 연결 직렬화는 한 프로세스 안에서만 유효하므로 다중 서버 인스턴스와 공유 파일시스템 운영은 지원하지 않는다.
- 이유: 별도 DB 서버 없이 현재 개발·단일 인스턴스 배포 범위에서 데이터 손실을 줄이고, 기존 저장소 포트를 유지하기 위해서다.

## 후속 결정 필요

- 다중 인스턴스용 서버 DB, 인증된 주체 기반 Task 인가, 감사 로그, 저장소 오류 계약과 동시성 제어의 도입 기준을 정한다.
- 운영 환경의 자동 백업 주기, 보존 기간, 암호화와 정기 복원 훈련을 결정한다.

## 출처

- `logs/STATE.md`
- `src/Domain/Task.hs`
- `src/Infrastructure/InMemoryTaskRepository.hs`
- `app/Main.hs`
- `src/Infrastructure/SQLiteDatabase.hs`
- `src/Infrastructure/SQLiteTaskRepository.hs`
- `src/Infrastructure/SQLiteUserRepository.hs`
