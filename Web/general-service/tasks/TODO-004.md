# TODO-004: 입력 검증과 도메인 오류 모델 확장

## 상태

`TODO`

## 현재 분석

현재 서버는 제목과 기대 결과물만 필수로 검증한다. Task Owner, Outcome Owner, 설명 길이, 중복·공백 정규화의 규칙은 없다.

## 목표

잘못된 데이터가 저장소와 UI 상태에 들어가기 전에 일관된 도메인 오류로 차단한다.

## 선행 작업

TODO-002

## 완료 조건

- [ ] 담당자 이름, 제목, 기대 결과물, Outcome 설명의 공백·길이·필수 규칙을 합의한다.
- [ ] 규칙을 `TaskError`와 도메인 검증 함수에 구현한다.
- [ ] API 오류 메시지와 Elm 폼 오류 표시가 해당 오류를 구분해 안내한다.
- [ ] 경계값과 공백 입력에 대한 테스트를 추가한다.

## 영향 범위

`src/Domain/Task.hs`, `src/Interface/Http/TaskRoutes.hs`, `web/src/Domain/Task.elm`, `web/src/Presentation/TaskBoard.elm`
