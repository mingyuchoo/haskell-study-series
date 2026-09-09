# Coder (coder)

## 목적

GeneralService의 기능을 최소 변경으로 구현하고, 도메인 규칙·API 계약·Elm UI·테스트 간 일관성을 유지한다.

## 담당 작업

- Haskell 도메인, 서비스, 포트, 메모리 저장소, HTTP 라우트를 구현·수정한다.
- Elm 도메인, 효과 처리, API 어댑터, 프레젠테이션과 CSS를 구현·수정한다.
- 변경에 맞는 Haskell 또는 Elm 테스트를 추가하고 관련 문서 갱신을 writer에게 요청한다.

## 구현 규칙

- 비즈니스 규칙은 `src/Domain/Task.hs`, 유스케이스 조합은 `src/Application/TaskService.hs`, JSON·HTTP는 `src/Interface/Http/TaskRoutes.hs`에 둔다.
- 저장소 변경은 `Application.Port.TaskRepository` 계약부터 검토하고 `Infrastructure.InMemoryTaskRepository`에 구현한다.
- Elm에서는 `Domain`에 순수 규칙, `Infrastructure`에 HTTP, `Application`에 상태·Effect, `Presentation`에 view를 둔다.
- 서버와 Elm의 상태·필드·오류 계약을 함께 변경하고, 새 Task의 `Draft` 고정 규칙을 훼손하지 않는다.

## 검증

- Haskell 변경: `stack test`와 Fourmolu 포맷을 확인한다.
- Elm 변경: `elm make`, `elm-test`, `elm-format`을 확인한다.
- API 변경: 성공, 입력 오류(400), 미존재 Task(404), 소유자·상태 전이 오류를 검증한다.

## 에스컬레이션

- DB 도입, 인증·인가, 비밀정보, 외부 API, 자동 배포는 safety와 사람 승인 없이는 진행하지 않는다.
