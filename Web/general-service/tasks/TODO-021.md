# TODO-021: 개발용 회원가입·로그인·프로필 관리 제공

## 상태

`REVIEW`

## 현재 분석

현재 GeneralService는 인증된 사용자 모델이나 세션이 없으며, 모든 사용자가 같은 관리자 화면과 API에 접근한다.

## 목표

사용자가 이메일·비밀번호·표시 이름으로 가입하고 로그인한 뒤, 자신의 표시 이름을 수정하거나 로그아웃할 수 있게 한다.

## 완료 조건

- [x] 비밀번호를 평문으로 저장하지 않고 bcrypt 해시로 저장한다.
- [x] 중복 이메일, 잘못된 로그인, 짧은 비밀번호, 유효하지 않은 프로필 입력을 API에서 거부한다.
- [x] 가입·로그인·로그아웃·내 프로필 조회 및 표시 이름 수정 API와 Elm 화면을 제공한다.
- [x] 서버 재시작 시 사용자와 세션이 사라지는 개발용 제약 및 운영 전제(HTTPS, 영속화, 속도 제한, 인가)를 문서화한다.
- [x] 핵심 도메인·서비스 흐름 테스트와 Haskell/Elm 빌드를 통과한다.

## 범위와 참고 자료

- `README.md`, `docs/PRODUCT.md`, `src/Domain/User.hs`, `src/Application/AuthService.hs`
- `src/Application/Port/UserRepository.hs`, `src/Infrastructure/InMemoryUserRepository.hs`
- `src/Interface/Http/TaskRoutes.hs`, `web/src/`

## 메모

- Task·Outcome의 서버 측 역할 인가는 기존 `TODO-017` 범위로 유지한다.
- 2026-09-10: 초기 모델이 `loading = True`인데 초기 인증 화면에서는 작업 목록 요청을 하지 않아 제출 버튼이 계속 비활성화됐다. 초기값을 `False`로 변경했다.
