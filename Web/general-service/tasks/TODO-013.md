# TODO-013: Outcome 생성·조회 UI 연결

## 상태

`TODO`

## 현재 분석

백엔드는 `GET/POST /api/outcome`과 승인 Task 조립 규칙을 제공하지만 Elm에는 Outcome 타입, API 호출, 생성 화면, 목록이 없다. 즉 API 기능이 사용자 흐름으로 완결되지 않았다.

## 목표

동일 Outcome Owner의 승인 Task를 선택해 Outcome을 만들고 결과를 조회하는 화면 흐름을 제공한다.

## 선행 작업

TODO-006, TODO-007, TODO-009, TODO-012

## 완료 조건

- [ ] Elm 도메인 모델·API 어댑터에 Outcome 조회·생성을 추가한다.
- [ ] 승인 상태와 같은 Outcome Owner를 기준으로 선택 가능한 Task만 제시한다.
- [ ] Outcome 설명, 소유자, 포함 Task, 결과물, `Effective` 상태를 목록 또는 상세 화면에 표시한다.
- [ ] 빈 선택·미승인 Task·다른 Owner 혼합 오류를 UI와 API에서 검증한다.

## 영향 범위

`web/src/Domain/Task.elm`, `web/src/Application/TaskBoard.elm`, `web/src/Infrastructure/TaskApi.elm`, `web/src/Presentation/TaskBoard.elm`
