# 작업 보드

| 작업 ID | 상태 | 담당 | 목표 |
| --- | --- | --- | --- |
| T-2026-0908-001 | DONE | orchestrator/coder | 업무 상태 CRUD 웹 서비스 구현 |
| T-2026-0908-002 | DONE | orchestrator/coder/reviewer | 전체 포맷·빌드·테스트·실행 스크립트 추가 |
| T-2026-0908-003 | DONE | orchestrator/coder/reviewer | Clean Architecture 기반 Haskell·Elm 리팩터링 |
| T-2026-0908-004 | DONE | orchestrator/coder | 업무 UI를 상태별 Kanban 보드로 개편 |
| T-2026-0908-005 | DONE | orchestrator/coder | 드래그 앤 드롭으로 업무 상태 변경 |
| T-2026-0908-006 | DONE | orchestrator/coder | 안내 메시지를 3초 자동 소멸 토스트로 변경 |
| T-2026-0908-007 | DONE | orchestrator/coder | 신규 업무를 초안 상태로 고정 등록 |
| T-2026-0908-008 | DONE | orchestrator/coder/reviewer | 업무 상태를 Draft/Reviewed/Submitted/Approved/Effective로 통일 |
| T-2026-0908-009 | DONE | orchestrator/coder/reviewer | run.sh 실행 전 기존 GeneralService 서버 종료 |
| T-2026-0908-010 | DONE | orchestrator/coder/reviewer | 아이젠하워 매트릭스 기반 업무 우선순위 추가 |
| T-2026-0908-011 | DONE | orchestrator/coder | 긴급도·중요도 선택을 라디오 버튼으로 전환 |
| T-2026-0908-012 | DONE | orchestrator/coder | Work 도메인 명칭을 Task로 전면 전환 |
| T-2026-0908-013 | IN_PROGRESS | orchestrator/coder | Task 결과물 리뷰·승인 및 Outcome 조립 서비스 구현 |

## 가정

- 초기 버전은 서버 프로세스 동안 유지되는 메모리 저장소를 사용한다.
- Elm 0.19.2(사용자 표기 1.9.2의 현행 배포 버전)를 사용한다.
- 새 업무의 기본 우선순위는 `NotUrgent` + `Important`(계획 수립)으로 설정한다.
