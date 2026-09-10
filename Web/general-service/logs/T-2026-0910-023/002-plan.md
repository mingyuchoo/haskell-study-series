task_id: T-2026-0910-023
from: planner
status: REVIEW
summary: "SQLite 스키마, 원자적 저장소 계약, 초기화와 검증 전략을 설계했다."
assumptions:
  - "기본 경로는 `general-service.sqlite3`이며 `DATABASE_PATH`로 재정의한다."
  - "메모리 저장소는 단위 테스트 대역으로 유지한다."
result: |
  - `sqlite-simple`과 공유 `MVar Connection`을 사용한다.
  - Task, Outcome 스냅샷, 사용자, 만료 가능한 세션 테이블을 버전 기반 전진 마이그레이션으로 생성한다.
  - 예시 Task는 시드 이력으로 최초 한 번만 추가한다.
  - 파일 DB를 닫고 다시 열어 모든 저장소의 영속성을 통합 테스트한다.
sources:
  - "tasks/TODO-005.md"
  - "tasks/TODO-006.md"
  - "src/Application/Port/TaskRepository.hs"
  - "src/Application/Port/UserRepository.hs"
open_questions: []
risks:
  - "저장소 오류와 동시 가입 경쟁을 표현하려면 포트 변경 범위가 커질 수 있다."
  - "SQLite는 공유 파일시스템 기반 다중 서버 운영에 적합하지 않다."
cost:
  tokens: 0
  tool_calls: 7
