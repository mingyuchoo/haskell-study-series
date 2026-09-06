# T-ACTIVITY: 활동 기록 화면 분리

## 확인한 사실
- 사용자 제공 AGENTS.md를 적용한다. 저장소 루트와 docs/, docs/plans/, static/, static/src/, static/src/Page/, src/에서 별도 AGENTS.md는 발견하지 못했다.
- Page.Learning.viewWith는 회고 UI 아래 전체 w.events를 audit-history 영역으로 표시한다.
- Page에는 Reviews 다음 Settings가 있으며 활동 기록 페이지는 없다.
- Api.Decode.auditDecoder는 API record.event 중 평가 보조 필드만 사용한다. 서버의 기존 이벤트에는 조직/구성원/목표/책임/권한/결과/회고/전략 사건이 있다.
- Ui.Guide의 감사 기록 링크는 Reviews audit-history를 가리킨다.

## 구현 계획
1. coder: Page에 Activity를 추가하고 Main의 사이드바에서 학습 다음 활동 기록 메뉴를 배치한다. 조직 진입/전환 및 새로고침 시 페이지·필터 상태를 기존 모델 정책에 맞춰 관리한다.
2. coder: Learning에서 전체 이벤트 목록을 제거하고 Page.Activity로 옮긴다. 기본은 표(시각, 변경 유형, 대상, 기록 주체, 내용)이며 기존 카드/표 규칙을 따른다. 검색, 유형 선택, 시작/종료일 필터 및 초기화/결과 없음 안내를 제공한다.
3. coder: 기존 API의 record.event를 Elm에서 구조적으로 읽어 유형·대상 ID·관련 회고/목표를 추출한다. 설명 문자열의 ID 치환에 의존하지 않는다. 현재 조직/구성원/목표 이름을 사용하되 없는 ID와 알 수 없는 이벤트는 원본을 보존하여 표시한다. 현재 이름이라는 성격을 명시하고 이름 변경 전 이름으로 오인하지 않게 한다.
4. coder: 시간대가 명시된 읽기 쉬운 시각을 표시한다. 새 의존성 없이 UTC 표기와 UTC 기준 기간 필터를 사용해 날짜 경계 일관성을 확보할 수 있다. 각 행 상세에 순번, 원본 시각, 원본 ID/이벤트를 제공한다. 기록 주체의 미인증 성격을 유지한다.
5. coder: 회고 카드/표에 해당 회고 활동 기록으로 연결하는 동작을 추가하고 대상 필터 또는 이벤트 선택으로 관련성을 보장한다. Ui.Guide 링크를 새 활동 기록 화면으로 연결하고 문구를 활동 기록으로 통일한다.

## 검증 및 완료 조건
- tester는 수정 완료 후 make test 및 필요한 Elm 회귀 검증을 수행한다. 새 메뉴와 기본 표, 학습에서 전체 기록 제거, 검색/유형/기간 조합, 기간 경계, 빈 결과, 알 수 없는 이벤트/누락 이름, 상세 원본, 가이드 및 회고 링크를 검증한다.
- coder는 빌드와 린트/포맷을 기존 명령으로 확인하고 생성 파일은 저장소 관례에 따른다.
- reviewer는 학습의 회고 기능 유지, 활동 설명의 실제 이벤트 의미, 기록 주체/시각 정확성, 조직 간 상태 혼합 여부, 접근성을 읽기 전용으로 검토한다.
- orchestrator는 tester 통과와 reviewer 승인 후 완료한다. 신규 의존성, 저장소 데이터 변경, git push는 수행하지 않는다.

## 확인이 필요한 구현 세부
- API record.event의 실제 직렬화 형태 및 OrganizationScoped 래핑 여부는 coder가 서버 JSON 인스턴스/fixtures로 확인한다.
- 원본 이벤트 확장으로 기존 Audit 테스트 fixture가 영향을 받으면 tester 담당 경로와 조율한다.
