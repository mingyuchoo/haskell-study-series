# GeneralService

회원가입·로그인한 구성원이 Task 상태를 등록·수정·삭제하는 Haskell + Elm 웹 서비스입니다.

## 실행

```bash
stack run
```

브라우저에서 `http://localhost:3000`을 엽니다.

## 기능

- 이메일·비밀번호·표시 이름으로 회원가입 및 로그인
- 로그인한 사용자의 표시 이름을 수정하는 내 프로필 관리와 로그아웃
- 업무 제목, 설명, 상태의 생성·조회·수정·삭제
- `Draft`, `Reviewed`, `Submitted`, `Approved`, `Effective` 상태를 한국어로 표시
- 긴급도·중요도 조합으로 계산한 아이젠하워 매트릭스 실행 분류(즉시 실행, 계획 수립, 위임, 제거)별 Task 목록 표시
- 실행 분류와 별개로 상태별 스윔레인(초안, 검토 완료, 제출됨, 승인됨, 효력 발생)에서 진행 상태를 관리하고 드래그 앤 드롭으로 상태 변경
- 실행 분류와 상태 스윔레인의 카드는 제목, Task Owner, 실행 분류, 진행 상태, 결과물 진행 상태, 설명 요약을 표시하며, 카드를 누르면 전체 내용을 담은 상세 패널이 열림
- 상세 패널은 실행 분류와 진행 상태를 각각 명확히 표시해 두 축을 혼동하지 않음
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
| `POST` | `/api/auth/signup` | 회원가입 (`email`, `displayName`, `password`) |
| `POST` | `/api/auth/login` | 로그인 (`email`, `password`) |
| `POST` | `/api/auth/logout` | 로그아웃 (Bearer 토큰) |
| `GET` | `/api/auth/me` | 내 프로필 조회 (Bearer 토큰) |
| `PUT` | `/api/auth/me` | 내 표시 이름 변경 (`displayName`, Bearer 토큰) |

비밀번호는 bcrypt 해시로만 저장합니다. 사용자·세션·Task 저장소는 현재 서버 메모리입니다. 서버를 다시 시작하면 계정과 로그인 세션, 초기 예시 Task가 초기화됩니다. 이 구현은 개발용이며 HTTPS, 영속 저장소, 속도 제한 및 서버 측 Task 권한 검증 없이 외부에 배포하면 안 됩니다.
