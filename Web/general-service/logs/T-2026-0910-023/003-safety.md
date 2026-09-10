task_id: T-2026-0910-023
from: safety
status: REVIEW
summary: "조건부 허용(L1). SQLite 구현과 신규·임시 DB 마이그레이션은 사용자 승인 범위다."
assumptions:
  - "기존 운영 DB에 직접 적용하거나 배포하지 않는다."
result: |
  전진 마이그레이션은 트랜잭션으로 실행하고 미래 스키마 버전은 시작을 거부한다.
  1회 시드 마커, DB 파일 Git 제외, 세션 만료, 연결 직렬화와 Outcome 트랜잭션을 적용한다.
  기존·공유 DB에 실제 적용할 때는 백업·복구 계획과 별도 사람 승인이 필요하다.
sources:
  - "policies/safety.md"
  - "tasks/TODO-006.md"
  - "docs/SECURITY.md"
open_questions: []
risks:
  - "실행 중 메모리 데이터는 자동 이관되지 않는다."
  - "DB 유출 시 개인정보와 활성 세션 토큰이 노출될 수 있다."
cost:
  tokens: 0
  tool_calls: 5
