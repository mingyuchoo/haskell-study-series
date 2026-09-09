# Product

## 목적

GeneralService는 관리자가 업무(Task)를 등록하고, 결과물을 제출·검토·승인한 뒤 여러 승인 업무를 하나의 Outcome으로 조립하는 내부 업무 관리 웹 서비스다.

## 사용자와 역할

| 역할 | 책임 |
| --- | --- |
| 관리자 | Task를 생성·수정·삭제하고 칸반 보드에서 상태를 관리한다. |
| Task Owner | 자신이 맡은 Task의 결과물을 제출한다. |
| Outcome Owner | 제출된 결과물을 승인하거나 수정 요청하고, 승인된 Task로 Outcome을 생성한다. |

## 핵심 사용자 흐름

1. 관리자가 제목, 기대 결과물, 담당자와 우선순위를 입력해 Task를 만든다. 새 Task는 항상 `Draft`로 시작한다.
2. Task Owner가 결과물을 제출하면 상태가 `Submitted`가 된다.
3. Outcome Owner가 결과물을 승인하면 `Approved`, 수정 요청하면 `Reviewed`가 된다.
4. 같은 Outcome Owner가 소유한 승인 Task들을 선택해 `Effective` 상태의 Outcome을 만든다.

## 범위와 가정

- 우선순위는 긴급도와 중요도를 조합한 아이젠하워 매트릭스로 계산한다.
- 현 버전은 인증·권한 관리가 없는 관리자용 단일 화면이다.
- 데이터는 서버 메모리에만 보관되므로 재시작하면 초기 예시 데이터로 초기화된다.

## 출처

- `README.md`
- `src/Domain/Task.hs`
