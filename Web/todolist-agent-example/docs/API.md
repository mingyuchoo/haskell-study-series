# HTTP API

기본 경로: `/api/v1`

## 생성

`POST /api/v1/todos`

요청:

```json
{"title":"Buy milk","description":"2 liters"}
```

성공: Todo 표현을 포함한 `201 Created`.

유효하지 않은 제목: 다음 본문과 함께 `400 Bad Request`.

```json
{"code":"INVALID_TITLE","message":"Todo title must be between 1 and 200 non-whitespace characters."}
```

## 목록 조회

`GET /api/v1/todos` -> `200 OK`

```json
{"items":[...]}
```

항목은 `createdAt` 오름차순으로 정렬된다.

## 단건 조회

`GET /api/v1/todos/:todoId` -> `200 OK` 또는 `404 Not Found`.
형식이 잘못된 UUID는 `400 Bad Request`를 반환한다.

## 완료

`POST /api/v1/todos/:todoId/complete` -> `200 OK` 또는 `404 Not Found`.
완료 요청은 멱등적이다.

## 삭제

`DELETE /api/v1/todos/:todoId` -> `204 No Content` 또는 `404 Not Found`.

## Todo JSON

```json
{
  "id":"550e8400-e29b-41d4-a716-446655440000",
  "title":"Buy milk",
  "description":"2 liters",
  "status":"active",
  "createdAt":"2026-09-09T00:00:00Z",
  "completedAt":null
}
```
