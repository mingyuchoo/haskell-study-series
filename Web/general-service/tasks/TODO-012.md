# TODO-012: Task 결과물 제출·검토 UX 보강

## 상태

`TODO`

## 현재 분석

상세 패널에서 제출, 승인, 수정 요청 API가 연결돼 있다. 다만 담당자 값이 단순 문자열이고, 허용 가능한 상태·다음 행동·검토 이력이 화면에서 충분히 구조화돼 있지 않다.

## 목표

Task Owner와 Outcome Owner가 결과물 처리 단계와 책임을 명확히 이해하도록 한다.

## 선행 작업

TODO-003, TODO-010, TODO-011

## 완료 조건

- [ ] 상태별로 가능한 행동과 다음 상태를 명확히 안내한다.
- [ ] 제출 결과물·검토 코멘트·처리 시각·처리자를 표시할 데이터 모델을 설계한다.
- [ ] 승인·수정 요청 실패 시 입력한 결과물 또는 코멘트를 보존한다.
- [ ] 제출·승인·수정 요청의 성공·실패를 Elm 및 인수 테스트로 확인한다.

## 영향 범위

`src/Domain/Task.hs`, `web/src/Application/TaskBoard.elm`, `web/src/Presentation/TaskBoard.elm`, `docs/UX.md`
