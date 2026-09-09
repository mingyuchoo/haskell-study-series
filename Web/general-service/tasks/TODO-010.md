# TODO-010: Elm API 효과와 오류 복구 강화

## 상태

`TODO`

## 현재 분석

`TaskApi.perform`은 Task CRUD와 결과물 워크플로 호출을 처리한다. 그러나 네트워크 오류는 모두 `RequestFailed`로 축약되고, 실패 뒤 새로고침·재시도·동시 수정 충돌 처리 전략이 없다.

## 목표

사용자가 API 실패의 원인을 이해하고 데이터를 잃지 않고 복구할 수 있게 한다.

## 선행 작업

TODO-007, TODO-009

## 완료 조건

- [ ] 네트워크, 시간 초과, 서버 오류, 검증 오류, 권한 오류를 구분해 표시한다.
- [ ] 저장·이동·워크플로 실패 시 입력과 선택 상태를 보존한다.
- [ ] 재시도 또는 목록 새로고침 흐름을 제공한다.
- [ ] 해당 상태 전이와 오류 메시지의 Elm 테스트를 추가한다.

## 영향 범위

`web/src/Application/TaskBoard.elm`, `web/src/Infrastructure/TaskApi.elm`, `web/tests/MainTest.elm`
