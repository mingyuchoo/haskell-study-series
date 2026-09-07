# GeneralService

관리자가 Task 상태를 등록·수정·삭제하는 Haskell + Elm 웹 서비스입니다.

## 실행

```bash
stack run
```

브라우저에서 `http://localhost:3000`을 엽니다.

## 기능

- 업무 제목, 설명, 상태의 생성·조회·수정·삭제
- `Draft`, `Reviewed`, `Submitted`, `Approved`, `Effective` 상태를 한국어로 표시
- 긴급도·중요도 조합으로 계산한 아이젠하워 매트릭스 우선순위(즉시 실행, 계획 수립, 위임, 제거) 표시
- 상태별 컬럼과 업무 카드로 구성된 Kanban 보드 및 드래그 앤 드롭 상태 변경
- 카드는 제목, 우선순위, 결과물 진행 상태, 설명 요약만 표시하고, 제목이나 "상세 보기"를 누르면 전체 내용을 담은 상세 패널이 열림
- 상세 패널에서 수정, 삭제와 함께 Task Owner의 결과물 제출, Outcome Owner의 승인과 수정 요청을 처리
- 신규 업무는 상태 선택 없이 항상 `초안`으로 등록
- 긴급도·중요도는 라디오 버튼으로 즉시 선택
- 작업 결과를 알려주는 3초 자동 소멸 토스트 알림
- 깊은 그린, 크림, 카드형 정보 구조의 따뜻한 카페 스타일 관리자 UI

## API

| 메서드 | 경로 | 설명 |
| --- | --- | --- |
| `GET` | `/api/task` | 업무 전체 조회 |
| `POST` | `/api/task` | 업무 생성 |
| `PUT` | `/api/task/:id` | 업무 수정 |
| `DELETE` | `/api/task/:id` | 업무 삭제 |
| `POST` | `/api/task/:id/submit` | Task Owner가 결과물 제출 (`taskOwner`, `submittedResult`) |
| `POST` | `/api/task/:id/approve` | Outcome Owner가 결과물 승인 (`outcomeOwner`, 선택 `reviewComment`) |
| `POST` | `/api/task/:id/revision` | Outcome Owner가 수정 요청 (`outcomeOwner`, 선택 `reviewComment`) |
| `GET` | `/api/outcome` | Outcome 전체 조회 |
| `POST` | `/api/outcome` | 승인된 업무를 묶어 Outcome 생성 |

저장소는 현재 서버 메모리입니다. 서버를 다시 시작하면 초기 예시 Task로 돌아갑니다.
