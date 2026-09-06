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
