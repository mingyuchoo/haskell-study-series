# T-20260907-001 코드베이스 의도 적합성 분석

- 작성일: 2026-09-07
- 담당: orchestrator (직접 분석)
- 검토: 생략 (.claude/agents/에 reviewer 정의가 없어 규약상 위임 불가)

## 가정
- 사용자 의도는 "자신이 속한 조직 정보를 입력하고, 그 위에서 멀티 AI 에이전트 시스템을 모델링하는 것"으로 해석했습니다.
- 소스 정적 분석과 로컬 빌드/테스트 실행에 근거하며, 사용자 저장소(runs/)는 변경하지 않았습니다.

## 검증 실행 결과
| 항목 | 결과 |
|---|---|
| stack test (1회차) | 119 examples, 1 failure (StartupSmokeSpec 데모 수명주기) |
| stack test (2회차) | 119 examples, 0 failures |
| 실패 테스트 단독 재실행 | 통과 |
| npm test (Elm) | 132 passed |
| npm run check (경계, 포맷, 타입) | 통과 |
| make lint (GHC -Werror) | 통과 |

## 1. 의도대로 구현된 부분

### 조직 정보 입력과 운영 모델
- 구성원(역할, 보고 관계, 부서, 이메일, 재직 상태), 목표(KPI, 기준/목표값, 기간, 상위 목표, 필요 권한/예산), 단일 최종 책임자, 7종 결정 권한과 예산 한도, 결과/평가/회고/전략이 도메인 타입으로 정의되어 있습니다.
- 활성화 불변식(책임자, 유효 KPI, 충분한 권한/예산), 권한 회수 시 초안 복귀, 구조 진단(O000~O051), 책임 그래프와 통제율이 순수 함수로 구현되어 있습니다.
- 조직 단위 격리, 논리 삭제, 낙관적 버전(409), 원자적 저장이 테스트로 보호됩니다.

### 조직 현황(Discovery)과 근거 검토
- 확인된 사실/미확인/개선안 구분, 확인된 사실의 근거 필수, 본문 변경 시 검토 상태 pending 복귀가 서버와 화면 양쪽에서 강제됩니다.
- 별도 이벤트(DiscoverySaved)로 저장되어 기존 목표/권한 데이터를 오염시키지 않습니다.

### 아키텍처
- Domain(순수, Aeson 없음) / Application(Command, Plan, Query) / Serialization / Http / Infrastructure 분리가 지켜지고 scripts/check-boundaries.cjs가 이를 강제합니다.
- Elm도 Domain/Form/Api/Page/Ui 경계를 검사합니다.

## 2. 의도 대비 미흡한 부분: 멀티 AI 에이전트 모델링

1. 에이전트가 도메인 개념으로 존재하지 않습니다. src/ 전체에 "agent" 문자열이 0건입니다. 에이전트 타입, 이벤트, 검증, 진단이 없습니다.
2. "규칙 기반 에이전트 초안"은 static/src/Page/Discovery.elm의 agentCard가 Workflow 필드를 그대로 카드로 나열하는 화면 표현입니다. 실제 규칙은 "role이 비면 업무명 + 담당" 정도입니다. 계획 문서 docs/plans/T-ORG-UX-002.md가 이렇게 범위를 정했으므로 계획대로는 구현되었으나, 시스템 모델링이라는 의도에는 미치지 못합니다.
3. 에이전트 간 관계를 표현할 수 없습니다. handoff, approval, tools, inputs, outputs가 모두 자유 텍스트라 인계 대상이 다른 업무/에이전트를 참조하지 않습니다. 오케스트레이터-서브에이전트, 파이프라인, 병렬 같은 토폴로지를 기록하거나 검증할 수 없습니다.
4. 조직 모델과 에이전트 설계가 연결되지 않습니다. Workflow.role은 Person을 참조하지 않고(참고 문구로 이름만 나열), approval은 Authority/Permission과 연결되지 않습니다. "이 업무의 승인자가 실제로 해당 권한을 보유하는가" 같은 진단이 불가능합니다. Domain/Compiler.hs는 Discovery를 전혀 읽지 않습니다.
5. 권한 등급과 도구 허용 목록 모델이 없습니다. CLAUDE.md의 L0~L3, 허용 도구 개념에 해당하는 필드가 없고 tools는 문자열입니다.
6. 데모 조직에 업무 흐름 시드가 없습니다. src/MyOrg/Demo.hs는 Discovery를 생성하지 않아 데모에서 에이전트 초안 화면이 비어 있고, 체험 가이드 4단계도 운영 흐름만 다룹니다.
7. 도출 결과를 외부 에이전트 정의(YAML, .claude/agents/*.md 등)로 내보내는 기능이 없어 설계가 실제 시스템으로 이어지지 않습니다.

## 3. 기타 발견
- 간헐적 테스트 실패: test/StartupSmokeSpec.hs의 데모 수명주기 테스트가 runs/demo/events.json.lock 잔존으로 1회 실패했습니다. runBootstrap이 1.5초 후 SIGINT로 종료하는 과정에서 잠금 정리가 경합하는 것으로 추정합니다. 데모 모드가 실제 포트 8081을 고정 사용하는 점도 환경 의존성입니다.
- 사용자 저장소 runs/demo/events.json.lock 디렉터리가 9월 6일 15:51부터 남아 있고 실행 중인 서버 프로세스는 없습니다. 이 상태에서 make demo는 시작에 실패합니다. README 지침대로 수동 정리가 필요합니다. (이번 분석에서 삭제하지 않았습니다.)
- README 드리프트: "다섯 화면"으로 설명하지만 실제 메뉴는 조직 진단, 업무 흐름, 구성원, 목표, 책임, 권한, 에이전트 초안, 결과, 학습, 활동 기록과 조직 목록, 조직 설정입니다. 에이전트 초안, 활동 기록, 조직 설정과 Discovery 관련 Elm 모듈이 README에 없습니다.
- STATE.md는 8개 작업만 기록하고 docs/plans의 9개 작업(T-ORG-UX-002, T-EMP-CRUD, T-RESP-GRAPH, T-ACTIVITY, T-LIST-VIEWS 등)은 기록되지 않았습니다. .claude/agents/와 policies/가 없어 CLAUDE.md 규약의 서브에이전트 위임과 reviewer 검토를 수행할 수 없습니다.

## 4. 권고 (우선순위 순)
1. Haskell 도메인에 AgentRole 타입을 추가합니다. 최소 필드: id, name, sourceWorkflow, inputs, outputs, tools(허용 목록), permissionLevel(L0~L3), approvalBy(UserId 또는 Permission 참조), handoffTo([AgentRoleId]). DiscoverySaved처럼 이벤트로 저장합니다.
2. 도출 규칙을 순수 함수(예: Domain.AgentDraft.derive :: Discovery -> OrgState -> [AgentDraft])로 옮기고 Compiler 방식의 진단을 추가합니다. 예: 인계 대상 미해결, 승인자 권한 불일치, 도구 미확인, 사람 승인 없는 되돌릴 수 없는 산출물.
3. Workflow.role, approval, handoff를 Person, Permission, Workflow ID 참조로 점진 전환하고 기존 문자열 데이터는 호환 유지합니다.
4. Ui/ResponsibilityGraph를 재사용해 에이전트 토폴로지(인계 그래프) 화면을 추가합니다.
5. 데모에 업무 흐름 3~4건을 시드하고, 초안을 YAML/Markdown으로 내보내는 기능을 추가합니다.
6. 간헐 실패 테스트는 stop 후 잠금 정리 확인 또는 데모 포트 주입으로 보강하고, README와 STATE.md를 현행화합니다.
