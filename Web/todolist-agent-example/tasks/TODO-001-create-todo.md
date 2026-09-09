# TODO-001 — Todo 생성

상태: 승인됨

## 목표
`POST /api/v1/todos`를 구현한다.

## 관련 명세
`PRODUCT.md`, `ARCHITECTURE.md`, `DATA_MODEL.md`, `API.md`, `SECURITY.md`, `ACCEPTANCE_TESTS.md`.

## 요구사항
- 필수 제목과 선택적 설명을 받는다.
- 제목의 앞뒤 공백을 제거하고 검증한다.
- UUID와 현재 UTC 시각을 생성한다.
- 초기 상태는 Active이며 완료 시각은 없다.
- PostgreSQL에 저장한다.
- 201을 반환한다.

## 비요구사항
수정, 인증, 태그, 우선순위, 기한, 페이지네이션은 포함하지 않는다.

## 테스트
유효한/빈/너무 긴 제목, 애플리케이션 포트를 통한 영속화, HTTP 201 및 400을 검증한다.

## 인수 기준
AT-001, AT-002.
