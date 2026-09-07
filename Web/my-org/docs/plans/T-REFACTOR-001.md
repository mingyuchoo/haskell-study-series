# T-REFACTOR-001: 순수 코어의 응집도와 외부 계약 경계 개선

## 요청과 확인한 사실

사용자는 앞선 분석의 1~4단계를 순서대로 구현하도록 승인했다. 기능, 사용자 표시 문구, HTTP 응답 및 저장 이벤트 JSON을 보존하면서 기존 순수 코어/부수효과 경계를 강화한다. 실제 저장소 및 상위 경로, docs/plans와 조사한 소스 경로에 AGENTS.md가 없어 사용자 제공 규약을 적용한다. planner는 계획 및 배정된 문서를 작성하고 소스를 수정하거나 셸을 실행하지 않는다.

현재 Application.executeCommand는 순수하지만 모든 업무 명령과 공유 검증을 함께 보유한다. Domain.Analysis와 Review는 판단과 표시 문구를 함께 보유하며 Compiler 진단의 상당 부분이 PlainMessage이다. Elm Main은 화면 상태, 조직별 초안, 서버 요청 수명, Cmd 생성과 화면 조립을 포함한다. Serialization.JSON은 저장 이벤트와 HTTP에 공용이며 명시적 key/tag를 통해 기존 형식을 유지한다.

## 보존할 불변식

- executeCommand와 planCommand, executeQuery는 시간과 데이터를 인자로 받는 순수 함수로 유지한다.
- Runtime의 동일 잠금 안에서 읽기/계획 → 영속화 → 메모리 공개 순서를 유지하고 저장 실패와 비동기 예외 처리를 보존한다.
- 이벤트의 전역 순번, 조직 범위, 구형 이벤트 replay, 조직 삭제/재생성 수명, 낙관적 버전 검사와 오류 우선순서를 유지한다.
- AddEmployee의 빈 프로필이 기존 PersonAdded 이벤트로 기록되는 규칙, 직원 비활성화 시 인계/권한/활성 목표 처리, 목표 활성화 검증을 유지한다.
- Elm의 오래된 응답 무시, 저장 중 중복 제출/편집/탐색 차단, 조직별 초안 보존, 실패 시 초안 보존과 조회 갱신, 자동 쓰기 재시도 금지를 유지한다.
- API의 HTTP 상태, 경로, JSON key/tag/선택 필드/null, 숫자와 문구를 유지한다. test/fixtures의 기존 파일은 기대 결과에 맞추기 위해 갱신하지 않는다.
- OrgState 통합 모델과 Runtime 원자성을 유지한다. 전면 패키지 분할 및 projection 캐시/증분 replay 같은 성능 변경은 제외한다.
- 새 의존성, 저장 데이터 마이그레이션, 배포, git push, 실제 사용자 저장소 접근은 범위에 포함하지 않는다.

## 단계 1: 명령의 업무별 응집도

1. Command 타입을 독립 모듈로 분리하고 기존 MyOrg.Application은 Command 재노출 및 얇은 executeCommand 디스패처로 유지한다. 기능 모듈은 파사드를 역참조하지 않는다.
2. 조직 수명주기, 직원/보고 관계/프로필, 목표/책임/권한, 결과/회고의 순수 명령 처리 함수를 책임별 모듈로 이동한다. 정확한 이름은 coder가 기존 네이밍에 맞추되 파일 분리 자체를 목표로 삼지 않는다.
3. 공통 식별자/텍스트/버전/조회 검증만 작은 명명된 함수로 공유하고 직원 고유 정책은 직원 모듈 안에 둔다. AddEmployee→AddPerson 및 GrantGoalAuthority→GrantAuthority의 executeCommand 재진입을 직접 검증 함수 재사용으로 바꾼다.
4. 같은 상태/명령/시간에 대해 동일한 Either 오류 또는 이벤트가 나오도록 검증 순서와 단일 이벤트 리스트 반환 계약을 유지한다.

완료 기준: 기존 명령/직원/계획 테스트 통과, 재진입 제거 확인, 순수 경계 검사 적용, 업무별 담당 코드가 실제로 분리됨. 새 테스트는 명령 재사용 경로의 성공과 충돌하는 검증 입력에서 오류 우선순위를 검증한다.

## 단계 2: 도메인 판단과 표시 문구

1. Analysis의 원인, 자원 요구, 권고 자원을 의미 있는 합 타입으로 표현하고 분석 함수는 구조화된 사실과 판단을 반환한다. 조직 ID/목표 설명/부족 자원/coverage 등 렌더링에 필요한 원자료를 타입에 담는다.
2. 기존 renderAnalysis와 원인/자원/권고 문구를 Presentation.Analysis로 이동하고 describeReviewWarning을 Presentation.Review로 옮긴다. Domain이 Presentation을 import하지 않도록 소비자를 변경한다.
3. Compiler의 진단 원인을 구조화하여 Presentation.Diagnostic이 기존 code/severity/subject/message/details를 동일하게 표현하도록 한다. 전체 진단 유형을 다루되 기존 InvalidDraft 의미를 잃지 않는다.
4. HTTP 표현은 기존 문구와 JSON을 유지한다. 표시 문자열을 구조화된 도메인 사실로 역추론하는 코드를 순수 업무 계층에 만들지 않는다. 기존 표시용 JSON 디코딩이 필요하면 경계 DTO로 유지하며 기존 텍스트 역파싱은 compatibility 경계에 한정할 수 있다.

완료 기준: 도메인 판단 테스트와 표시/JSON 계약 테스트를 별도로 보강하고 기존 dashboard fixture 및 오류/진단/회고 출력이 동일함을 확인한다. 레거시 저장 이벤트를 읽는 데 표시용 타입 변경이 영향을 주지 않는다.

## 단계 3: Elm 상태 책임과 효과 의도

1. Main을 Browser 연결, Cmd 해석, 화면 조립 중심으로 줄인다. 서버 수명(요청 세대, fresh/saving/syncing, 응답 처리), 조직별 초안과 식별자/버전 관리, 페이지 로컬 상태를 작은 순수 모듈로 분리한다.
2. 순수 전이 함수가 ( Model, List Effect )와 같은 명시적 결과를 반환하게 한다. Effect는 조직 목록 조회, 특정 조직 workspace 조회, 저장 요청, DOM 이동/포커스 등 현재 실제 효과를 데이터로 표현한다. Cmd와 함수 콜백 자체를 Effect 값에 저장하지 않는다.
3. Main 또는 전용 Runtime 모듈의 해석기가 Effect를 Api.Http/Browser.Dom 호출로 변환하고 결과 메시지에 현재의 요청 토큰과 Action을 보존한다. Http.Error의 사용자 문구 변환은 경계 또는 순수 어댑터에 위치시키며 순수 상태 모듈이 Api.Http를 import하지 않는다.
4. 초안 저장 키, 인물 수정 버전 보존, 목표 serial 생성, 삭제 Snapshot, 가이드/그래프 이동의 현재 동작을 유지한다. 공개 Main API를 사용하는 테스트는 순수 전이 API를 직접 검증하도록 갱신할 수 있으나 기존 행동 검증을 삭제하지 않는다.
5. 경계 검사에 새 순수 모듈 네임스페이스를 추가하고 Api.Http/Http/Browser/Task 및 Main 역의존을 금지한다. 효과 표현이 JSON payload를 보유하면 테스트는 필요한 필드를 decode하여 의미를 검증한다.

완료 기준: 기존 상태 테스트와 화면 테스트 통과. stale 응답/중복 submit은 효과가 없고, 정상 submit은 정확히 한 번 쓰기, 실패 후 조회만 발생, 조직 전환 조회의 조직/토큰 및 가이드 이동 효과가 맞는지 테스트한다. 상태 Tuple.first만 검증하지 않는다.

## 단계 4: 저장 이벤트와 HTTP 코덱 계약

1. 기존 명시적 JSON 형태를 유지하면서 저장 전용 이벤트 코덱과 HTTP DTO/코덱의 진입점을 분리한다. FileStore/SQLiteStore는 저장 코덱만, Http.Route/Encode/Server는 HTTP 코덱만 참조한다.
2. 공용 기본 JSON 도구와 안정된 원시 값 표현은 하위 모듈로 공유할 수 있다. 그러나 저장 코덱이 HTTP 또는 Presentation의 표시 규칙에 의존하거나 단지 이름만 다른 래퍼로 기존 전체 모듈을 가져오지 않게 한다.
3. 타입 클래스 인스턴스의 중복/전역 선택 문제를 피하도록 저장과 HTTP 계약에 서로 다른 wrapper 또는 명시적 함수/DTO를 사용한다. orphan instance 경고를 끄는 방식으로 경계를 만들지 않는다. 기존 Serialization.JSON은 필요할 때만 호환 파사드로 유지하고 신규 어댑터는 이를 사용하지 않는다.
4. 저장 이벤트의 구형/조직 범위 형식과 명시적 생성자 태그를 그대로 유지한다. HTTP에 렌더링되는 분석/진단 DTO와 저장에 실제로 필요한 타입을 구분한다.
5. 경계 검사에 저장→HTTP/Presentation 및 HTTP→저장 구현 의존 금지를 추가하고 실제 import 그래프로 확인한다.

완료 기준: legacy-events/scoped-events/dashboard fixture 불변. File/SQLite 재개방과 audit/projection 복구, HTTP wire 계약과 decoder, 잘못된 입력/구형 형식 처리를 모두 통과한다.

## 순서, 담당, 경로 잠금

각 단계를 순차 구현한다. 단계마다 coder 구현 완료 후 tester와 reviewer가 병렬로 검증하며, 둘 다 통과하면 다음 단계로 진행한다. 단계 2의 타입 변경과 단계 4의 코덱 경계가 충돌하지 않도록 coder 한 명이 소스 잠금을 소유한다. tester만 test/ 및 static/tests/를 수정하고 coder에게 실패를 보고한다. reviewer는 읽기 전용이다. planner는 docs/plans/와 추가 배정된 README.md를 수정한다.

orchestrator가 실제 저장소 경로 기준으로 coder에게 src/, static/src/, scripts/check-boundaries.cjs, generated my-org.cabal/static/app.js를 배정했다. 같은 파일을 동시에 수정하지 않는다. 외부 서비스 호출은 이 작업에 필요하지 않다.

## 검증과 최종 완료

- 단계별로 관련 Haskell/Elm 테스트와 경계 검사를 실행하고 각 코드 변경 후 규약상 make test를 실행한다. 실패 원인을 수정한 뒤 해당 검증을 다시 확인한다.
- 최종 make test, make lint, npm run build, git diff --check를 수행한다. 프런트 산출물 static/app.js를 소스와 일치시킨다.
- JSON fixture는 diff가 없어야 하며 저장 동시성/실패/초기화 검증을 보존한다. 테스트는 임시 파일과 로컬 시험 프로세스만 사용한다.
- reviewer는 순수성뿐 아니라 실제 책임 분리, 우회 import, 명령 오류 순서, 요청 세대 및 저장/HTTP 독립성을 검토한다.
- 최종 결과에 변경 책임, 테스트 결과와 남은 제한을 기록한다. 예상 못한 실패가 같은 작업에서 3회 계속되면 규약에 따라 사람에게 보고한다.

## 단계별 완료 기록

아래 실행 결과는 orchestrator가 전달한 tester 결과와 reviewer 승인을 기록한 것이다. planner는 문서만 수정했다.

| 단계 | 구현 결과 | 검증 | 리뷰 및 계약 |
| --- | --- | --- | --- |
| 1 | Application.Command.Types와 업무별 Organization/People/Goals/Authority/Review/Validation 분리, 명령 재진입을 직접 검증 재사용으로 교체 | Elm 113개, Haskell 109개 통과 | reviewer 승인, 기존 JSON fixture 불변 |
| 2 | Analysis/Compiler의 구조화된 판단과 Presentation.Analysis/Diagnostic/Review의 표시 모델·문구 분리 | Elm 113개, Haskell 113개 통과 | reviewer 승인, 기존 JSON fixture 불변 |
| 3 | App.Session/Drafts/PageState 상태 책임 분리, App.Update의 순수 전이와 App.Effect 데이터, Main의 효과 실행 경계 분리 | Elm 118개, Haskell 113개 통과 | reviewer 승인, 기존 JSON fixture 불변 |
| 4 | Serialization.Codec와 기능별 값/이벤트 코덱, Serialization.Persistence 저장 API, Http.Codec 및 표시 코덱 분리 완료; JSON은 호환 파사드로 유지 | Elm 118개, Haskell 115개 통과 | reviewer APPROVE, 기존 JSON fixture 불변 |

3단계 브라우저 smoke는 Aside 데몬을 사용할 수 없고 CUA가 제공하는 브라우저 목록도 비어 있어 수행하지 못했다. 테스트용 임시 서버는 정리되었다. 이를 자동 테스트 통과와 구분하여 남긴다. README의 아키텍처 표와 설명은 1~4단계 구현을 반영했다. 전체 1~4단계가 구현·검증·코드 리뷰를 완료했다.

## 최종 검증 결과

- make test 통과: Elm 118개, Haskell 115개. 작업 시작 기준 Elm 5개, Haskell 10개 테스트를 추가했다.
- make lint의 -Werror 빌드 종료 코드 0. 의존 경계·전이 의존성, 포맷, 빌드 및 git diff --check 검사 통과.
- 기존 JSON fixture 3개와 package-lock.json의 SHA256이 작업 전과 동일하며 package.yaml 변경 없음. 저장 형식과 HTTP 계약 및 의존성을 유지했다.
- 1~4단계 모두 reviewer 승인. 브라우저 연결이 없어 실제 브라우저 smoke는 수행하지 못했으며 앞서 기록한 자동 테스트와 구분한다.
