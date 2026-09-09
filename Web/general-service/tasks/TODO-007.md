# TODO-007: HTTP API 계약과 오류 응답 표준화

## 상태

`TODO`

## 현재 분석

`TaskRoutes`는 JSON API를 제공하지만 오류 문자열이 라우트에 직접 분산되어 있고, 응답 스키마·버전·상태 코드의 계약 테스트가 없다.

## 목표

클라이언트와 외부 소비자가 안정적으로 사용할 수 있는 일관된 API 계약을 만든다.

## 선행 작업

TODO-003, TODO-004

## 완료 조건

- [ ] 성공·검증 오류·권한 오류·미존재 오류의 공통 JSON 스키마를 정한다.
- [ ] `/api/task`와 `/api/outcome`의 요청·응답 예제를 API 문서에 작성한다.
- [ ] 상태 코드와 JSON 직렬화·역직렬화에 대한 라우트 테스트를 추가한다.
- [ ] 호환성이 깨지는 변경의 버전 전략을 결정한다.

## 영향 범위

`src/Interface/Http/TaskRoutes.hs`, `README.md`, `docs/ARCHITECTURE.MD`, API 테스트
