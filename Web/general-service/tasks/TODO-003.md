# TODO-003: 도메인 상태 전이 무결성 강제

## 상태

`TODO`

## 현재 분석

결과물 제출·승인·수정 요청은 도메인 함수로 검증하지만, 일반 수정과 칸반 드래그는 `updateTask`를 통해 임의 상태를 저장할 수 있다. 따라서 `Draft`에서 곧바로 `Approved`로 이동할 수 있다.

## 목표

상태 변경이 정의된 업무 흐름과 담당자 검증을 우회하지 못하게 한다.

## 선행 작업

TODO-002

## 완료 조건

- [ ] 상태 전이 표와 금지 전이를 도메인 함수로 구현한다.
- [ ] 일반 정보 수정과 상태 변경을 분리하거나, 상태 변경에 전이 검증을 적용한다.
- [ ] 드래그 앤 드롭, HTTP `PUT`, 제출·승인·수정 요청이 같은 전이 규칙을 따르도록 한다.
- [ ] 허용·거부 전이에 대한 Haskell 단위 테스트를 추가한다.

## 영향 범위

`src/Domain/Task.hs`, `src/Application/TaskService.hs`, `src/Interface/Http/TaskRoutes.hs`, `web/src/Application/TaskBoard.elm`
