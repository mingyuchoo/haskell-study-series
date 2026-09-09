# 아키텍처 결정

## ADR-001 PostgreSQL
상태: 승인됨. 프로덕션 영속성 계층은 내구성 있는 트랜잭션 저장소로 PostgreSQL을 사용한다.

## ADR-002 UUID 식별자
상태: 승인됨. 공개 식별자로 순서나 데이터베이스 구현 세부 정보가 드러나지 않도록 Todo ID에 UUID를 사용한다.

## ADR-003 도메인 독립성
상태: 승인됨. 도메인 모듈은 Servant, Aeson 또는 PostgreSQL에 의존하지 않는다.

## ADR-004 v1에서는 재개방하지 않음
상태: 승인됨. 버전 1에서는 완료된 Todo를 Active 상태로 되돌릴 수 없다.

## ADR-005 명시적 리포지터리 포트
상태: 승인됨. 애플리케이션 유스케이스는 `Todo.Application.TodoRepository`에 의존하며, PostgreSQL 어댑터가 해당 기능 레코드를 구현한다.
