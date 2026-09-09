# TODO-009: Elm 도메인 모델과 서버 계약 동기화

## 상태

`TODO`

## 현재 분석

Elm은 Haskell과 별도로 `Status`, `Urgency`, `Importance`, `Task`, `TaskInput`을 정의한다. 현재 Task 응답의 `quadrant` 필드는 Elm에서 다시 계산하고, Outcome 타입은 아직 없다.

## 목표

서버와 클라이언트의 타입·열거형·검증·오류 표현이 계약에 따라 같이 진화하도록 한다.

## 선행 작업

TODO-002, TODO-004, TODO-007

## 완료 조건

- [ ] 서버 응답의 필드별 소유 계층(서버 계산값 또는 UI 계산값)을 명시한다.
- [ ] Elm에 Outcome과 표준 API 오류 모델을 추가할 준비를 한다.
- [ ] 알 수 없는 열거형과 누락 필드에 대한 디코더 실패를 사용자 친화적으로 처리한다.
- [ ] Haskell JSON 예제와 Elm 인코더·디코더를 대조하는 테스트를 추가한다.

## 영향 범위

`web/src/Domain/Task.elm`, `web/src/Infrastructure/TaskApi.elm`, `src/Interface/Http/TaskRoutes.hs`
