task_id: T-2026-0910-023
from: orchestrator
status: IN_PROGRESS
summary: "Task·Outcome·사용자·세션 저장소를 SQLite로 전환한다."
assumptions:
  - "사용자의 SQLite 사용 요청을 저장소 기술 선택과 로컬 마이그레이션 구현에 대한 사람 승인으로 본다."
  - "기존 데이터베이스 파일은 삭제하거나 덮어쓰지 않고 전진 마이그레이션만 적용한다."
  - "기존 메모리 저장소는 빠른 단위 테스트용 구현으로 유지한다."
result: |
  기본 데이터베이스 파일은 프로젝트 루트의 `general-service.sqlite3`로 설정하고
  `DATABASE_PATH` 환경 변수로 경로를 바꿀 수 있게 한다. 빈 데이터베이스에만 예시 Task를 시드한다.
sources:
  - "tasks/TODO-005.md"
  - "tasks/TODO-006.md"
  - "policies/safety.md"
open_questions: []
risks:
  - "스키마 변경과 백업·복구 절차를 문서화해야 한다."
  - "현재 저장소 포트는 DB 오류를 도메인 오류로 표현하지 않는다."
cost:
  tokens: 0
  tool_calls: 5
