# TODO-002 — Todo 목록 및 단건 조회

상태: 승인됨

## 목표
목록 조회와 단건 조회를 구현한다.

## 요구사항
- `GET /api/v1/todos`는 오래된 Todo부터 반환한다.
- `GET /api/v1/todos/:todoId`는 Todo 하나를 반환한다.
- 형식이 잘못된 UUID -> 400.
- 존재하지 않는 유효한 UUID -> 404.

## 인수 기준
AT-003, AT-006.
