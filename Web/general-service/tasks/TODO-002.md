# TODO-002: Task·Outcome 도메인 계약을 명세와 코드에서 고정

## 상태

`TODO`

## 현재 분석

`src/Domain/Task.hs`에 Task, Outcome, 담당자, 상태, 우선순위 사분면이 정의되어 있으나 필드와 상태 전이의 정식 계약은 코드와 문서에 흩어져 있다.

## 목표

도메인 타입·상태값·소유자 책임·Outcome 조립 조건을 모든 계층의 기준으로 확정한다.

## 선행 작업

TODO-001

## 완료 조건

- [ ] `docs/DATA_MODEL.md`에 필수·선택 필드, 상태값, Outcome 포함 규칙을 코드와 일치하게 갱신한다.
- [ ] `Draft`, `Reviewed`, `Submitted`, `Approved`, `Effective`의 의미와 허용 전이를 정의한다.
- [ ] API·Elm에서 사용하는 문자열이 Haskell 도메인 열거형과 일대일로 대응함을 테스트한다.

## 영향 범위

`src/Domain/Task.hs`, `docs/DATA_MODEL.md`, `docs/DECISIONS.md`, `web/src/Domain/Task.elm`
