# Changelog for `my-org`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- 에이전트 역할을 도메인 타입(`Domain.Agent`)과 `AgentRolesSaved` 이벤트로 추가했습니다. 권한 등급 L0~L3, 사람 승인 주체, 허용 도구, 인계 대상을 기록합니다.
- 저장된 업무 흐름에서 역할 후보를 결정적으로 도출하는 `Domain.AgentDraft.deriveAgents`와 A001~A009 진단을 추가했습니다. `GET/POST /api/organizations/:id/agents`가 초안, 저장된 설계, 진단을 제공합니다.
- 업무 흐름에 담당 구성원, 승인 구성원, 승인 결정 권한, 인계 대상 업무 참조 필드를 추가했습니다. 비어 있으면 JSON에 나타나지 않아 기존 기록과 호환됩니다.
- 에이전트 초안 화면의 설계안 편집과 저장, 에이전트 구조 화면의 인계 구조 SVG, `GET /api/organizations/:id/agents/export` Markdown 정의 파일 내보내기를 추가했습니다.
- 데모 시드에 현황 항목 3건과 참조 연결이 있는 업무 흐름 4건을 추가하고 체험 가이드에 다섯 번째 단계를 추가했습니다. 고정 fixture와 구형 데모 인식은 기존 52개 이벤트 형식을 유지합니다.
- 조직 현황과 업무 흐름을 근거·확인 상태와 함께 저장하는 조직별 discovery API와 이벤트 모델을 추가했습니다. 기존 저장 기록은 빈 현황 문서로 시작합니다.
- 현재 조직 입력부터 규칙 기반 에이전트 구성 초안 검토까지 이어지는 안내, 필드 도움말과 일반 조직 진행 가이드를 추가했습니다.
- 버전 충돌 시 덮어쓰기를 거부하고 현황 본문 변경 시 초안 검토 상태를 다시 확인하도록 처리합니다.

### Changed

- 서버가 SIGINT와 SIGTERM을 받으면 저장소 잠금을 해제한 뒤 종료합니다. 시작 스모크 테스트는 일회용 서버 정지 후 남은 잠금을 정리합니다.
- README의 화면 목록과 모듈 표를 현재 메뉴 구성에 맞게 갱신했습니다.
- 선택 PostgreSQL 저장소를 SQLite(`sqlite-simple`, `MY_ORG_SQLITE_FILE`)로 교체했습니다. 기본 JSON 저장, 기존 이벤트 형식과 데모의 전용 JSON 저장은 유지합니다.
- SQLite 이벤트 저장은 트랜잭션과 연결 수명 동안의 독점 잠금을 사용합니다. 기존 JSON 또는 PostgreSQL 데이터를 자동 변환하지 않습니다.
- README에 SQLite 실행 방법, 저장소 선택 우선순위 및 잠금 동작을 추가했습니다.

## 0.1.0.0 - YYYY-MM-DD
