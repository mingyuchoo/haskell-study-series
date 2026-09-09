# GeneralService 에이전트 운영 규약

## 프로젝트 맥락

GeneralService는 Task의 등록·결과물 제출·검토·승인과 Outcome 조립을 제공하는 Haskell + Elm 웹 서비스다. 백엔드는 WAI/Warp와 메모리 저장소를 사용하고, 프런트엔드는 Elm 단일 페이지 앱이다. 도메인 규칙은 `src/Domain/Task.hs`가 단일 진실 원천이다.

## 작업 시작과 완료

1. `logs/STATE.md`에서 진행 중인 작업과 가정을 확인한다. 새 작업은 오케스트레이터가 이 보드에 등록한다.
2. 관련 문서(`docs/`)와 Task 카드(`tasks/`)를 읽고, 변경 전에 영향받는 Haskell·Elm 계층을 확인한다.
3. 역할 문서에 정의된 범위 안에서 작업하고, 다른 역할의 전문 판단은 위임한다.
4. 코드 변경은 최소 범위로 적용하고 해당 테스트를 추가·수정한다.
5. 검토자가 정확성·완전성·형식·정책 준수를 승인한 뒤에만 `DONE`으로 기록한다.

## 코드베이스 규칙

- 의존성 방향은 `Interface → Application → Domain ← Infrastructure`를 지킨다. 도메인은 HTTP, WAI, Elm, 저장소 구현을 import하지 않는다.
- `TaskItem`의 상태 전이, 소유자 검증, Outcome 생성 규칙은 도메인 함수로 구현한다. HTTP 라우트나 Elm 화면에 규칙을 복제하지 않는다.
- API JSON 열거형은 `Draft`, `Reviewed`, `Submitted`, `Approved`, `Effective`를 유지한다. UI의 한국어 레이블은 별도 표현 책임이다.
- 새 Task는 항상 `Draft`로 생성한다. Task Owner만 결과물을 제출하고 Outcome Owner만 검토하며, Outcome에는 같은 Outcome Owner의 승인 Task만 포함한다.
- 저장소는 현재 메모리 기반이다. 영속성·인증·외부 호출을 추가하는 작업은 안전 검토와 사람 승인이 필요하다.

## 검증 명령

| 목적 | 명령 |
| --- | --- |
| Haskell 테스트 | `stack test` |
| Elm 테스트 | `cd web && npx --no-install elm-test` |
| Elm 빌드 | `cd web && elm make src/Main.elm --output=elm.js` |
| 포맷 | `scripts/format.sh` |
| 전체 로컬 검증·실행 | `scripts/run.sh` |

`scripts/run.sh`는 포트의 기존 GeneralService 프로세스를 종료할 수 있으므로, 실행 전 포트와 영향 범위를 확인한다.

## 산출물 형식

에이전트는 아래 형식으로 오케스트레이터에게 결과를 반환한다.

```yaml
task_id: T-YYYY-MMDD-NNN
from: agent-id
status: REVIEW # REVIEW, BLOCKED, NEEDS_HUMAN
summary: "핵심 결과"
assumptions: []
result: "변경 파일과 검증 결과"
sources: []
open_questions: []
risks: []
cost:
  tokens: 0
  tool_calls: 0
```

## 역할과 정책

- 역할별 지침: `agents/*.md`
- 안전·권한 규칙: `policies/safety.md`
- 문체·코드·문서 형식: `policies/style.md`
- 외부 전송, 배포, 영구 삭제, 자격 증명 입력, 결제는 사람의 명시적 승인 없이는 수행하지 않는다.
