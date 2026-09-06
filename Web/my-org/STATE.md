# 작업 보드

## T-2026-0906-001
- 요청: test/ Python 테스트를 모두 Haskell로 전환
- 상태: DONE
- 담당: coder (구현), writer (README), orchestrator (통합), reviewer (검토 승인)
- 가정: 기존 회귀 검증 범위를 유지하고 stack test로 실행한다.
- 결과: Python 5개 삭제, Haskell 스모크 모듈 5개 및 공용 지원 모듈 추가. 기존 테스트 유지. 애플리케이션 소스 변경 없음.
- 검증: coder 실행 stack test --ghc-options=-Werror — 70 examples, 0 failures; QuickCheck 100회 통과. git diff --check 통과.
- 검토: reviewer APPROVE (accuracy/completeness/format/policy_compliance 모두 pass)
- 제약: 시작 프로세스 회수 테스트는 POSIX 환경 필요. README 반영.

## T-2026-0906-002
- 요청: .gitignore 현행화
- 상태: DONE
- 담당: coder (규칙 수정/검증), reviewer (검토), orchestrator (통합)
- 가정: Haskell 생성 산출물과 로컬 파일만 제외하고 소스·잠금 파일·작업 기록은 유지한다.
- 선행 결과: 기존 규칙은 .stack-work/, runs/, *~이다.
- 결과: Stack/Cabal/GHC 생성물, 로컬 환경 및 편집기 임시 파일 제외 규칙 추가. 기존 runs/ 제외 유지.
- 검증: git check-ignore --no-index로 생성물 26개 제외 및 소스/설정/잠금/기록/환경 예제 12개 추적 유지 확인. git diff --check 통과.
- 검토: reviewer APPROVE (모든 항목 pass)
- 최종 상태: DONE

## T-2026-0906-003
- 요청: static 웹 UI를 Elm으로 재구축
- 상태: DONE
- 담당: coder (Elm 구현), orchestrator (빌드 통합/브라우저 검증), reviewer (검토 승인)
- 가정: 기존 Haskell JSON API와 기능을 유지하며 Elm Architecture로 상태와 폼을 관리한다. 사용자 저장 데이터는 변경하지 않는다.
- 결과: static/src/Main.elm 및 Domain.elm, Elm 의존성, 최적화 app.js, 최소 bootstrap.js, 반응형 UI. npm 고정 툴체인/Makefile/Haskell 정적 제공 및 배포 소스 목록/README 통합.
- UX: 명시적 조회·저장·실패 상태, 조직별 초안 보존, 요청 경합·중복 제출 방어, 버전 충돌·삭제 이름 확인, 접근성 및 Escape 취소.
- 검증: make build 및 npm run check 통과. stack test --ghc-options=-Werror 70 examples/0 failures, QuickCheck 100회 통과. git diff --check 통과.
- 브라우저 검증: 격리 저장소에서 조직 생성/수정, 구성원/목표 생성, 실패 후 입력/metricId 보존, 책임/권한/활성화/결과/평가/회고 저장 및 데모 4/4 완료. 삭제 이름 검사와 취소/Escape, 모바일 390px 가로 넘침 없음, 서버 종료 후 연결 오류 상태 표시 확인.
- 검토: reviewer 최종 APPROVE (모든 항목 pass).
- 제약: 초안은 앱 내 이동·조회 동안 유지되며 전체 페이지 재로드/종료 시 사라짐. 날짜 입력 UTC. 브라우저에서 실제 삭제는 실행하지 않음(기존 Haskell 삭제 회귀 테스트 통과).
- 정리: 검증 서버/브라우저 종료. 사용자 데이터와 분리된 runs/elm-ui-check/ 사용.

## T-2026-0906-004
- 요청: 메뉴를 목표·책임·권한·결과·학습으로 재구성
- 상태: DONE
- 담당: coder (화면 재구성), orchestrator (통합/문서), reviewer (검토)
- 가정: 조직 목록은 유지하고 결과 보고·평가를 결과 화면으로 분리하며 회고·학습은 학습 화면에 배치한다.
- 결과: 조직 목록 유지, 목표·책임·권한·결과·학습 순서 적용. 결과 보고·평가·이력 Results 화면 분리, 목표↔결과 바로가기 및 데모 03 연결 갱신. README/컴파일 app.js 반영.
- 검증: npm run format/build/check 및 git diff --check 통과. 브라우저 메뉴·결과/학습 화면·초안 보존·양방향 이동·데모 결과 이동 확인. 오류 로그 없음.
- 검토: reviewer APPROVE (모든 항목 pass).

## T-2026-0906-005
- 요청: make build에서 frontend도 함께 빌드
- 상태: DONE
- 가정: 이미 있는 build: frontend 의존성을 유지하고 실제 실행을 검증한다.
- 담당: orchestrator (실행 확인), reviewer (검토)
- 확인: frontend는 npm ci 및 npm run build, build는 frontend 이후 stack build를 실행한다.
- 결과: Makefile은 이미 요청을 충족하여 추가 수정 없음. 실제 make build 종료 코드 0: npm ci → Elm 최적화 빌드 → stack build 통과. reviewer APPROVE.

## T-2026-0906-006
- 요청: 순수/부수효과 분리 및 높은 응집도·낮은 결합도를 위한 코드베이스 분석과 의견
- 상태: DONE
- 담당: orchestrator (백엔드 분석/통합), researcher (Elm 분석), reviewer (근거 검토)
- 가정: 구현 변경 없이 현재 로컬 소스에 근거하여 점진적 리팩토링 방향을 제안한다.
- 결과: Haskell executeCommand/replay/evaluate 및 Elm update의 순수성 확인. 개선 우선순위는 Main Elm의 API·폼·화면 응집도, Store의 planner/원자 실행/저장 어댑터 경계, Server의 typed query/read model/HTTP 경계. Types/이벤트 직렬화 분리는 후순위.
- 검토: reviewer APPROVE. State/Validation/Reducer 의존 방향 및 JSON 순수성 설명 보정. 저장 원자성/로그 호환과 프런트 상태 불변식 보존 권고.
- 제약: 소스 정적 분석이며 이번 작업에서 코드를 변경하거나 테스트를 재실행하지 않음.

## T-2026-0906-007
- 요청: 순수/부수효과 분리 리팩토링 1~5단계 순차 실행
- 상태: DONE
- 가정: 기존 HTTP API/이벤트 저장 형식/화면 동작 유지. 사용자 저장소 수정 없이 임시 저장소로 검증한다.
- 담당: orchestrator (순서/통합/백엔드 검증), coder (단계별 구현), reviewer (각 단계 검토)
- 1단계: DONE — Haskell73/QuickCheck100, Elm21 통과. 고정 legacy/scoped 이벤트·HTTP fixture 및 상태/decoder 회귀 추가, 교차 reviewer APPROVE.
- 2단계: DONE — Elm API·typed Goal/Review 폼·화면 분리. Elm31/경계 검사/최적화 빌드, 브라우저 초안 보존 검증 및 reviewer APPROVE.
- 3단계: DONE — Plan/Runtime/Persistence/FileStore/PostgresStore 분리. Haskell80/QuickCheck100 및 reviewer APPROVE. 실제 PostgreSQL 통합은 미실행.
- 4단계: DONE — typed Query/ReadModel 및 Http.Route/Encode 분리. Haskell84/QuickCheck100, 고정 JSON 유지 및 reviewer APPROVE.
- 5단계: DONE — 업무별 타입 및 Event.Types/State/Queries/Validation/Reducer 분리. Domain Aeson 제거, 명시 Wire codec 및 표현 경계, Haskell90/QuickCheck100 및 reviewer APPROVE.

- 최종 통합: make build 및 make test 통과 (Elm31, Haskell90/0, QuickCheck100). 단계별 GHC -Werror, 의존 경계/포맷/타입 검사 및 git diff --check 통과.
- 호환 검증: 기존 legacy/scoped 이벤트와 dashboard 고정 JSON 3개 SHA256 불변, 읽기/재개방/API 회귀 통과.
- 검토: 단계별 교차 검토 및 최종 reviewer APPROVE.
- 제약: 실제 PostgreSQL 서버 통합은 미실행. Haskell 직접 Aeson 호출은 명시 codec으로 이전 필요. 진단 JSON은 기존 문자열 projection을 유지하며 내부 오류 타입은 역복원하지 않음.
- 정리: 사용자 저장소 변경 없음. 격리 브라우저/검증 서버 종료. 구조와 실행 지침 README 반영.

## T-0005
- 요청: 기본 JSON 저장을 유지하고 선택 PostgreSQL 저장소를 SQLite로 교체하며 관련 문서 갱신
- 상태: DONE
- 담당: orchestrator (배정/통합), planner (계획/문서), coder (저장소/빌드 설정), tester (회귀 검증), reviewer (검토)
- 결정: MY_ORG_SQLITE_FILE이 설정된 일반 모드만 SQLite 사용. 미설정은 기존 JSON, 데모는 항상 전용 JSON. 기존 데이터 자동 변환 없음.
- 결과: sqlite-simple, 이벤트 append 트랜잭션, SQLite EXCLUSIVE 잠금과 DELETE journal. 과거 작업의 PostgreSQL 설명은 당시 구현 이력이며 현재 전환 대상임.
- 문서: README 실행·저장·테스트·아키텍처 안내 및 CHANGELOG 갱신.
- 검증: make test 통과 (Haskell 99개, Elm 31개), make lint 및 git diff --check 통과. 기존 JSON fixture 변경 없음. 경고 수정 후 SQLite 관련 8개 테스트를 -Werror로 실행하여 통과.
- 검토: reviewer 최종 APPROVE, 추가 수정 요구 없음.
