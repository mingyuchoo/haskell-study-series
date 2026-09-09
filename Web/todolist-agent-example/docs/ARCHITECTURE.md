# 아키텍처 명세

## 스타일

경량 포트-어댑터/계층형 아키텍처를 사용한다.

    HTTP Interface -> Application -> Domain
                         ^
                         |
                  Repository port
                         ^
                         |
              PostgreSQL infrastructure

## 도메인

`src/Todo/Domain/`에는 순수 엔티티, 불변 조건, 상태 전이가 포함된다. 이 계층은 Servant, Aeson 또는 PostgreSQL을 import해서는 안 된다.

## 애플리케이션

`src/Todo/Application/`에는 유스케이스와 `TodoRepository` 포트가 포함된다. 유스케이스는 효과를 명시적인 레코드로 받아 인메모리 구현으로 테스트할 수 있다.

## 인프라스트럭처

`src/Todo/Infrastructure/Persistence/`는 PostgreSQL을 애플리케이션 리포지터리 포트에 맞게 어댑트한다. SQL 행 표현은 이 계층의 비공개 구현으로 유지한다.

## 인터페이스

`src/Todo/Interface/HTTP/`는 Servant 라우트, 전송 DTO, 오류 매핑 및 WAI 애플리케이션을 정의한다.

## 조합

`app/Main.hs`는 설정을 로드하고 프로덕션 리포지터리와 HTTP 애플리케이션을 조합한다. 비즈니스 규칙은 `Main`에 두지 않는다.

## 타입 설계

- `TodoId`는 UUID 기반 `newtype`이다.
- `TodoTitle`은 오직 `mkTodoTitle`을 통해서만 생성한다.
- `TodoStatus`는 `Active | Completed`이며, 도메인 코드에서 `Bool`이나 자유 형식 `Text`를 사용하지 않는다.
- 완료는 순수 상태 전이이다.
