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

현재 Task와 Outcome은 서버 프로세스의 메모리 저장소에 존재하며, 재시작 뒤 유지되지 않는다.

## 출처

- `src/Domain/Task.hs`
- `src/Interface/Http/TaskRoutes.hs`
