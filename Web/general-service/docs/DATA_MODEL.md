# Data Model

## Task

| 필드 | 타입 | 설명 |
| --- | --- | --- |
| `taskId` | 정수 | Task 식별자 |
| `title` | 문자열 | 필수 제목 |
| `description` | 문자열 | 상세 설명 |
| `status` | 열거형 | 업무 진행 상태 |
| `urgency` / `importance` | 열거형 | 아이젠하워 우선순위 입력값 |
| `quadrant` | 계산값 | `DoFirst`, `Schedule`, `Delegate`, `Eliminate` |
| `taskOwner` | 문자열 | 결과물 제출 담당자 |
| `outcomeOwner` | 문자열 | 검토·Outcome 담당자 |
| `expectedResult` | 문자열 | 필수 기대 결과물 |
| `submittedResult` | nullable 문자열 | 제출 결과물 |
| `reviewComment` | nullable 문자열 | 승인 또는 수정 요청 코멘트 |

## Outcome

| 필드 | 타입 | 설명 |
| --- | --- | --- |
| `outcomeId` | 정수 | Outcome 식별자 |
| `outcomeDescription` | 문자열 | 필수 Outcome 설명 |
| `outcomeOwner` | 문자열 | Outcome 책임자 |
| `taskIds` | 정수 목록 | 포함된 승인 Task의 식별자 |
| `results` | 문자열 목록 | 각 Task의 제출 결과물 |
| `status` | `Effective` | 생성 완료 Outcome의 상태 |

## 열거형과 규칙

- 상태: `Draft` → `Submitted` → `Approved` 또는 `Submitted` → `Reviewed` → `Submitted`.
- 우선순위: 긴급·중요는 `DoFirst`, 비긴급·중요는 `Schedule`, 긴급·비중요는 `Delegate`, 비긴급·비중요는 `Eliminate`다.
- Outcome에는 동일한 Outcome Owner가 소유하고 `Approved` 상태인 Task만 포함할 수 있다.

## 저장 방식

실행 서버는 SQLite 파일을 사용한다. 기본 경로는 `general-service.sqlite3`이고 `DATABASE_PATH`로 재정의할 수 있다. Task, Outcome, 사용자와 유효 세션은 정상적인 서버 재시작 뒤에도 유지된다.

| 테이블 | 저장 내용과 제약 |
| --- | --- |
| `tasks` | Task 필드. `id`는 자동 증가 기본 키이고 상태·긴급도·중요도는 `CHECK` 제약으로 허용 문자열을 제한한다. |
| `outcomes` | Outcome 설명, 소유자, `Effective` 상태. |
| `outcome_task_ids` | Outcome에 포함된 Task ID와 순서를 저장한다. 원본 Task 행의 외래 키가 아니라 Outcome 생성 당시 선택 목록이다. |
| `outcome_results` | Outcome 생성 당시 제출 결과물과 순서를 스냅샷으로 저장한다. |
| `users` | 고유 이메일, 표시 이름, bcrypt 비밀번호 해시를 저장한다. |
| `sessions` | Bearer 토큰, 사용자 외래 키, 만료 시각을 저장한다. 세션 유효 기간은 생성 후 24시간이다. |
| `schema_migrations` | 적용된 전진 마이그레이션 버전과 적용 시각을 저장한다. |
| `app_metadata` | 예시 Task 최초 1회 시드와 같은 애플리케이션 메타데이터를 저장한다. |

Outcome의 Task ID와 결과물은 각각 `position` 순서로 복원한다. Outcome 저장은 본문, Task ID, 결과물 행을 하나의 트랜잭션에서 생성한다. 단일 프로세스 안의 DB 접근은 공유 연결과 `MVar`로 직렬화한다.

`SEED_EXAMPLE_DATA`가 활성화된 상태에서 Task가 비어 있으면 예시 Task를 한 번 시드한다. Task가 이미 있으면 시드를 건너뛰고 처리 이력만 남긴다. 운영 환경에서는 `SEED_EXAMPLE_DATA=false`로 시드를 비활성화하며, 기존 메모리 저장소 데이터의 자동 이관은 제공하지 않는다.

## 출처

- `src/Domain/Task.hs`
- `src/Interface/Http/TaskRoutes.hs`
- `src/Infrastructure/SQLiteDatabase.hs`
- `src/Infrastructure/SQLiteTaskRepository.hs`
- `src/Infrastructure/SQLiteUserRepository.hs`
