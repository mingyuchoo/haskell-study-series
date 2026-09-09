# TODO-005: 저장소 포트와 메모리 구현의 동시성·오류 경계 정비

## 상태

`TODO`

## 현재 분석

`InMemoryTaskRepository`는 `MVar`로 목록 변경을 보호하지만, 포트는 실패 원인·트랜잭션 경계를 표현하지 않고 Outcome 생성 전 조회와 생성이 분리돼 있다.

## 목표

향후 영속 저장소로 교체해도 도메인·애플리케이션 계층이 안정적으로 동작하도록 저장소 계약을 정비한다.

## 선행 작업

TODO-003, TODO-004

## 완료 조건

- [ ] 저장소 실패, 낙관적 동시성 또는 원자적 Outcome 생성에 필요한 계약을 설계한다.
- [ ] Outcome 대상 조회·검증·저장을 하나의 저장소 연산 또는 명확한 트랜잭션 경계로 만든다.
- [ ] 메모리 구현에서 ID 생성·삭제·동시 갱신의 동작을 테스트한다.
- [ ] 포트 변경의 이유를 ADR에 기록한다.

## 영향 범위

`src/Application/Port/TaskRepository.hs`, `src/Application/TaskService.hs`, `src/Infrastructure/InMemoryTaskRepository.hs`
