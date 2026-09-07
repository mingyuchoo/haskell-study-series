# T-ORG-UX-002: 조직 현황에서 멀티 에이전트 구성까지 안내

## 요청과 확인 사실
- 사용자가 목적/첫 화면, 화면별 안내, 공통 입력 도움말, 미확인 정보 구분, 업무 흐름 및 에이전트 구조 도출의 1~5 전체 구현을 승인했다.
- 확인: Elm 화면은 App.Update/App.Drafts/App.Session과 Page/Ui 계층으로 나뉘며 Haskell은 이벤트 기반 OrgState를 사용한다. 일반 조직에는 기존 Ui.Guide가 노출되지 않는다.
- 확인: 저장소와 확인한 상위/대상 디렉터리에는 AGENTS.md가 없으므로 사용자 제공 규약을 적용한다. 외부 의존성 추가, 실제 LLM 연결, 운영 DB 접근은 필요하지 않다.
- 해석: 이번 도출은 입력된 업무 정보를 근거로 만드는 규칙 기반 제안이며 실제 자율 에이전트 실행은 범위에 포함하지 않는다.

## 구현 순서와 완료 기준
1. 목적과 시작 흐름: 조직 목록/헤더에서 현재 조직 입력 → 구조 검토 → 에이전트 초안 도출을 설명한다. 입력 결과와 다음 행동이 드러나도록 한다. 신규 조직의 첫 진입도 현황 수집으로 연결한다.
2. 기존 각 화면 안내: 구성원은 실제 역할/보고 관계, 목표는 현재 목표/측정 기준, 책임은 최종 책임자, 권한은 현재 보유 권한, 결과/학습은 운영 이후 단계임을 안내한다. 각 화면에 짧은 목적, 예시, 다음 단계와 빈 상태 행동을 제공한다.
3. 공통 폼: 필드별 상시 도움말과 예시를 제공하고 input id와 aria-describedby를 연결한다. 목표 지표 선택은 기존 지표 선택/새 지표 흐름을 지원하여 ID 직접 입력 부담을 줄이고 공유 지표 분석 의미를 유지한다. 일반 조직에도 단계별 가이드를 제공하고 데모 전용 이름/ID에 의존하지 않도록 한다.
4. 별도 현황 수집 모델: 조직별 분석 범위/기준 시점과 현황 항목을 지속 저장한다. 항목은 확인된 사실/미확인/개선안 구분과 근거를 가진다. AI 추론은 생성 결과에 별도 표시한다. 미확인 책임자·권한은 미확인 상태로 남길 수 있다. 기존 운영용 목표 활성화·권한 검증 규칙은 유지하며 현황 완료 기준으로 강요하지 않는다. 새로고침과 조직 전환 후에도 저장 내용과 미확인 구분이 유지되어야 한다.
5. 업무 및 도출 화면: 업무 이름, 시작 조건, 담당 역할/관련 구성원, 입력 정보, 사용 도구, 산출물, 전달 대상/인계, 사람 승인 조건, 근거와 확인 상태를 기록·수정한다. 저장된 업무별로 역할/도구/입력/출력/인계/사람 승인/입력 근거를 포함하는 규칙 기반 에이전트 초안을 제공한다. 입력 근거, 미확인 사항, 제안 내용을 시각적으로 분리한다. 사용자는 초안 검토 상태와 수정 의견/개선안을 저장할 수 있다. 입력 변경 이후 검토 상태는 재검토가 필요함을 알 수 있어야 한다.

## 구현 설계 제약
- 조직 현황/업무 모델을 기존 이벤트 저장 흐름에 통합하여 기존 데이터 및 이벤트 재생과 호환한다. 추가 필드의 이전 응답/기록 기본값은 빈 값으로 해석한다.
- 폼 제출 실패 시 입력을 보존하고 조직 ID/워크스페이스 버전 처리와 기존 저장 상태 규칙을 따른다.
- 에이전트 제안은 규칙 기반이라고 명시하고 외부 AI 실행/권한 발급이 일어났다고 표시하지 않는다.
- 조사 정보를 담은 별도 모델과 운영용 목표/권한 간 자동 덮어쓰기를 피한다. 미확인 항목이 있어도 탐색과 초안 검토는 가능하되 빈 근거를 확정 사실로 표시하지 않는다.
- 신규 의존성은 추가하지 않는다. 담당 coder는 src 및 실제 프런트엔드 소스 static/src를 구현 영역으로 배정받아 작업한다. tester는 실제 테스트 디렉터리 test 및 static/tests를 담당한다.

## 검증 및 인계
- tester: 현황/업무 저장과 재조회, 기존 이벤트 호환, 조직 격리, 미확인과 없음 구분, 도출 근거/사람 승인 표시, 입력 변경 후 재검토, 일반 조직 단계 안내, 기존 목표 활성화 회귀를 테스트한다.
- make test를 실행하고 npm run build 및 필요한 검사로 배포용 프런트엔드를 확인한다. 기존 테스트를 약화하거나 삭제하지 않는다.
- reviewer: 1~5 요구 충족, 사용자 데이터 보존, 도출의 정직한 표현, 접근성 연결, 공백/오류/새로고침 흐름과 변경 범위를 독립 검토한다.
- orchestrator: 구현 완료 후 tester/reviewer 통과를 확인하고 기능 및 검증 결과와 남은 제한을 한국어로 보고한다.

## 병행 구현을 위한 확정 API 계약
- GET /api/organizations/:id/discovery → { version: Int, discovery: Document }. 기존 dashboard 응답은 변경하지 않는다.
- POST /api/organizations/:id/discovery → { expectedVersion: Int, discovery: Document }. 기존 조직 이벤트 버전으로 충돌을 검증하고 문서 저장 이벤트 한 건을 추가한다. 응답은 기존 명령 성공 형식을 따른다. 저장 뒤 dashboard/discovery를 재조회한다.
- Document(JSON): { scope: String, asOf: String, observations: [Observation], workflows: [Workflow], review: Review }
- Observation: { id: String, subject: String, detail: String, status: String, evidence: String }. status 허용값은 confirmed/unknown/proposed. 화면 표기는 확인된 사실/미확인/개선안. subject는 책임/권한 등을 포함하는 자유 텍스트로 시작한다.
- Workflow: { id: String, name: String, role: String, trigger: String, inputs: String, tools: String, outputs: String, handoff: String, approval: String, status: String, evidence: String }. status 허용값은 confirmed/unknown/proposed. 목록 필드는 MVP에서 줄바꿈 가능한 문자열로 입력하고 보존한다.
- Review: { status: String, note: String }. status 허용값은 pending/reviewed. 초기값 pending, note 빈 문자열. scope/asOf/observations/workflows 변경 시 프런트엔드와 서버가 pending으로 되돌린다. review만 수정하는 저장은 reviewed를 허용한다.
- 기본 문서는 scope/asOf 빈 문자열, observations/workflows 빈 배열, review={status:"pending",note:""}. GET이 기존 조직에서 항상 기본 문서를 반환하도록 하여 이전 이벤트/fixture를 보존한다.
- 규칙 기반 초안은 프런트엔드에서 저장된 Workflow별 한 역할 후보로 결정적으로 생성한다. role/name → 역할, 나머지 필드 → 시작/입력/도구/산출물/인계/사람 승인, evidence/status → 근거와 미확인 표기. 빈 필드는 미확인으로 표시한다. 생성물 표시는 항상 규칙 기반 제안/추론이며 사용자 입력 확정과 별개다. 업무 목록이 비었으면 입력 안내를 표시한다.
- 백엔드 담당 잠금: src/** 및 필요한 빌드 모듈 목록만. 프런트엔드 담당 잠금: static/src/**, static/styles.css(실제 스타일 파일 확인 후). 테스트 담당 잠금: test/**, static/tests/**. 경계 교차 변경은 orchestrator 인계 후 수행한다.
