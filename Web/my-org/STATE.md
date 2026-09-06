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
