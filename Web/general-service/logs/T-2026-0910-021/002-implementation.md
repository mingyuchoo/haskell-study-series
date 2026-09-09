task_id: T-2026-0910-021
from: coder
status: REVIEW
summary: "bcrypt 기반 가입·로그인, Bearer 세션 기반 프로필 API, Elm 인증·프로필 UI를 구현했다."
assumptions:
  - "사용자와 세션은 개발용 메모리 저장소에만 보관한다."
result: |
  - `src/Domain/User.hs`, `src/Application/AuthService.hs` 및 메모리 UserRepository를 추가했다.
  - `POST /api/auth/signup`, `POST /api/auth/login`, `POST /api/auth/logout`, `GET/PUT /api/auth/me`를 추가했다.
  - 로그인 전 가입/로그인 화면과 로그인 후 표시 이름 수정·로그아웃 UI를 추가했다.
  - README, PRODUCT, TODO-021에 제약과 API를 기록했다.
sources: []
open_questions:
  - "Task·Outcome API의 서버 측 역할·소유자 인가는 TODO-017에서 다룬다."
risks:
  - "서버 재시작 시 사용자와 세션이 초기화되며, 외부 배포 전 HTTPS·영속 저장소·속도 제한이 필요하다."
cost:
  tokens: 0
  tool_calls: 12
