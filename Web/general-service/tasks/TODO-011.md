# TODO-011: Task CRUD 폼의 사용자 입력 경험 완성

## 상태

`TODO`

## 현재 분석

생성·수정·삭제와 상세 패널은 구현되어 있으나, 폼 검증은 제목 위주이고 서버 오류와 필드 오류를 연결하지 않는다. 삭제의 확인·복구 방식도 정의돼 있지 않다.

## 목표

Task를 빠르고 안전하게 생성·수정·삭제할 수 있는 폼 흐름을 완성한다.

## 선행 작업

TODO-004, TODO-010

## 완료 조건

- [ ] 필드별 필수·형식 오류와 서버 오류를 가까운 위치에 표시한다.
- [ ] 수정 취소와 저장 실패가 기존 입력을 보존한다.
- [ ] 삭제는 의도치 않은 실행을 막는 확인 또는 복구 가능 흐름을 제공한다.
- [ ] 새 Task가 UI와 서버 모두에서 `Draft`로 생성되는지 테스트한다.

## 영향 범위

`web/src/Application/TaskBoard.elm`, `web/src/Presentation/TaskBoard.elm`, `web/styles.css`, `web/tests/MainTest.elm`
