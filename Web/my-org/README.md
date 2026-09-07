# My Org

**현재 조직을 이해하고, 근거를 검토하며, 멀티 AI 에이전트 구성을 설계합니다.**

Haskell API와 Elm 웹 UI로 현재 조직의 역할·책임·권한과 업무 흐름을 정리하는 로컬 웹 MVP입니다. 입력한 업무에서 에이전트 구성 초안을 제안하고 근거와 미확인 사항을 검토합니다. 기존 목표 → 최종 책임자 → 결정 권한 → 결과 → 평가 → 학습 기능은 조직 운영을 지원합니다.

## 실행

서버는 기존 Stack/GHC 툴체인을 사용합니다. 프런트엔드 빌드에는 Node.js와 npm이 필요하며 `package-lock.json`에 고정한 Elm 0.19.1 컴파일러를 사용합니다. 최초 빌드에는 npm 및 Elm 패키지 저장소 접근이 필요합니다. Haskell 의존성은 `package.yaml`과 기존 Stack snapshot으로 관리합니다.

```sh
make build
make test
make run
```

브라우저에서 http://127.0.0.1:8080 을 엽니다. 저장소 루트에서 실행해야 `static/` 파일을 찾을 수 있습니다. 기본 저장소는 `runs/local/events.json`입니다. `MY_ORG_EVENT_FILE`과 `MY_ORG_PORT` 환경 변수로 변경할 수 있습니다. `.env` 파일은 읽지 않습니다.

첫 화면은 **조직 목록**입니다. 조직을 등록하거나 **데모 추가**를 누르고, 조직 카드의 **조직 열기 →**로 작업 공간에 들어갑니다. 여러 일반 조직과 데모를 함께 사용할 수 있으며 사람·목표·권한·회고·감사 기록은 조직별로 분리됩니다. 사이드바의 **조직 목록**으로 돌아가 다른 조직을 선택합니다.

조직 카드의 **상세 · 수정 · 삭제**에서 이름을 수정하거나 조직을 논리 삭제합니다. ID와 생성 시각은 바뀌지 않습니다. 삭제해도 다른 조직은 그대로 남고 원본 감사 기록은 저장소에 보존됩니다.

## 조직 현황과 에이전트 구성

조직별로 분석 범위와 기준 시점을 정하고, 구성원·목표·책임·권한을 확인합니다. 아직 모르는 책임이나 권한은 **현황 항목의 미확인 상태**로 남길 수 있습니다. 확인된 사실에는 근거를 기록하고 개선안은 현재 사실과 구별합니다. 현황 조사 때문에 실제 책임자나 권한을 임의로 지정하거나 목표를 활성화할 필요는 없습니다.

업무 흐름에는 업무 이름, 담당 역할, 시작 조건, 입력 정보, 사용 도구, 산출물, 인계 대상, 사람 승인 조건을 기록합니다. 저장된 업무마다 에이전트 역할 후보를 만드는 **규칙 기반 초안**을 제공합니다. 실제 LLM 호출이나 에이전트 실행, 도구 접근 권한 발급은 하지 않습니다. 비어 있는 업무 정보는 미확인으로 표시합니다. 입력 근거를 검토하고 검토 의견을 저장할 수 있으며 현황 본문을 바꾸면 검토 상태가 다시 `pending`이 됩니다.

현황은 기존 이벤트 저장소에 조직별로 저장됩니다. 기존 조직은 빈 문서에서 시작하며 원래 목표·권한 데이터는 변경하지 않습니다. 저장한 내용은 서버 재시작 후에도 복원되며, 아직 저장하지 않은 입력은 브라우저 종료 시 사라집니다.

- `GET /api/organizations/:id/discovery`: `{version, discovery}`를 반환합니다. 버전은 문서와 함께 읽은 조직 이벤트 버전입니다.
- `POST /api/organizations/:id/discovery`: `{expectedVersion, discovery}` 문서 전체를 저장합니다. 버전 충돌은 HTTP 409이며 자동 덮어쓰지 않습니다.
- `discovery`는 `scope`, `asOf`(빈 값 또는 `YYYY-MM-DD`), `observations`, `workflows`, `review`를 포함합니다. 현황·업무의 `status`는 `confirmed`/`unknown`/`proposed`, 검토 상태는 `pending`/`reviewed`입니다.
- `confirmed` 항목은 근거가 필요합니다. 제목과 ID는 비어 있을 수 없고 각 목록의 ID는 고유해야 합니다. 현황과 업무는 각각 최대 200건, 텍스트는 필드당 최대 10,000자입니다.

## Elm 프런트엔드

`static/src/`의 Elm 소스가 UI의 원본입니다. `static/app.js`는 최적화 컴파일 산출물이므로 직접 수정하지 않습니다. `static/bootstrap.js`는 Elm 앱을 시작하는 경계이며 화면과 API 상태는 Elm이 관리합니다.

```sh
make frontend     # npm ci 후 Elm 최적화 빌드
npm run check     # 모듈 의존 경계, Elm 표준 포맷 및 타입 검사
npm run format    # Elm 소스 표준 포맷 적용
```

`make build`는 프런트엔드와 Haskell 서버를 함께 빌드합니다. `make run`과 `make demo`는 체크인된 `static/app.js`를 제공하므로 Elm 소스를 수정한 뒤에는 `make frontend`를 먼저 실행하세요. `node_modules/`와 `elm-stuff/`는 로컬 캐시이며 Git에 포함하지 않습니다.

입력 초안은 조직별로 분리되며 화면 전환과 앱 안의 새로고침 요청에서 유지됩니다. 브라우저 전체 새로고침·종료 시에는 사라집니다. 날짜 입력은 화면에 명시한 UTC 기준입니다. 저장 중 중복 요청을 막고, 이전 조회 응답을 무시하며, 실패 시 입력을 보존합니다.

화면의 설계 기준은 [Elm Architecture](https://guide.elm-lang.org/architecture/)입니다. Model에 화면·입력·요청 상태를 보관하고, Msg를 update에서 처리하며, view가 현재 상태를 표현합니다. [공식 HTTP 예제](https://guide.elm-lang.org/effects/http.html)처럼 비동기 응답을 메시지로 받아 로딩과 실패를 구분합니다. JSON 디코더가 API 경계를 검증하고 기존 Haskell 도메인 검증이 저장 규칙을 최종 판단합니다.

| Elm 모듈 | 책임 |
| --- | --- |
| `Main` | Browser 연결, 화면 조립과 `Effect`를 실제 HTTP·DOM `Cmd`로 해석 |
| `App.Model`, `App.Update`, `App.Effect` | 책임별 상태의 조합, 순수 상태 전이와 실행할 효과의 데이터 표현 |
| `App.Session` | 조직 선택, 조회 요청 세대, 최신 여부와 저장 진행의 수명 관리 |
| `App.Drafts`, `App.PageState`, `App.Config` | 조직별 초안·버전·식별자, 페이지 로컬 상태와 시작 설정 |
| `Domain`, `Domain.Permission` | 화면에서 쓰는 데이터와 권한 종류 |
| `Form.Goal`, `Form.Review` | 타입으로 구분한 입력 필드·초안과 순수 검증 |
| `Api.Command`, `Api.Decode`, `Api.Path` | JSON 명령 생성, 응답 해석과 경로 구성 |
| `Api.Http` | HTTP 요청을 `Cmd msg`로 표현하고 응답을 호출자 메시지로 전달 |
| `Page.*`, `Ui.*` | 필요한 데이터와 콜백만 받아 화면 구성 |

`App.Update.update`는 다음 상태와 `List Effect`를 반환합니다. 조회·저장·포커스 의도를 `LoadOrganizations`, `LoadWorkspace`, `SaveCommand`, `FocusElement`로 표현하고 `Main.perform`이 실행합니다. HTTP 오류의 표시 문자열 변환도 Main 경계에서 처리하므로 순수 전이는 브라우저 없이 검증할 수 있습니다.

페이지는 전체 `App.Model`에 의존하지 않습니다. Goal/Review의 입력 이벤트는 필드 타입으로 연결하며 조직·구성원 등 단순 폼은 공통 문자열 입력을 사용합니다. `npm run check`의 의존 경계 검사가 도메인·폼에서 화면/API를 참조하거나 페이지에서 HTTP를 직접 호출하는 변경을 막습니다.

## 테스트

`make test`는 Elm 테스트·포맷·타입 검사 후 Haskell 테스트를 실행합니다. `npm test`로 Elm 회귀 테스트만, `stack test`로 `test/`의 Haskell 테스트만 실행할 수 있습니다. Hspec·QuickCheck 기반의 도메인 검증과 함께 API 생명주기, 동시 데모 초기화, 조직 삭제·재생성, 다중 조직 격리, 실제 서버 시작 로직의 재시작과 저장 상태 복원을 검증합니다.

통합 테스트는 임시 JSON 파일과 SQLite DB를 사용하며 필요한 서버를 직접 시작하고 종료합니다. SQLite 저장·재개방, 배치 저장의 원자성, 단일 서버 잠금, 손상된 데이터 거절 및 실제 시작 설정의 저장소 선택을 검증합니다. 별도로 서버를 실행하거나 외부 데이터베이스를 준비할 필요가 없습니다.

서버 프로세스 관리에 POSIX 기능을 사용하므로 Linux/macOS 등 POSIX 환경이 필요합니다.

Elm 테스트는 요청 경합, 중복 제출, 조직별 초안 보존, 수정·삭제 버전 확인과 JSON 응답 형식을 검증합니다. 상태 변화와 함께 생성된 효과도 검사하여 오래된 응답·중복 제출에 요청이 추가되지 않는지, 저장 실패 시 쓰기를 재시도하지 않고 조회만 갱신하는지 확인합니다. `static/tests/run.cjs`는 큰 개발/CI 머신에서 메모리를 과도하게 사용하지 않도록 테스트 worker를 최대 2개로 제한합니다.

`test/fixtures/`에는 기존 이벤트 로그와 HTTP 대시보드의 고정 JSON 계약이 있습니다. 테스트는 fixture를 자동 갱신하지 않습니다. 의도적으로 계약을 바꾸는 경우에만 루트에서 `test/fixtures/generate.sh`를 실행하고 변경 내용을 검토하세요. 정상 리팩토링에서는 fixture가 그대로 유지되어야 합니다.

## 다섯 화면

- **목표**: 구성원·목표 생성, KPI, 상위 목표, 책임자 지정, 활성화와 전략 기록.
- **책임**: 결과별 단일 최종 책임자, 필요 권한, 예산과 통제율. 사람·목표·지표·자원의 그래프 관계.
- **권한**: 구성원의 결정 권한·예산 변경과 집중도. 집중도는 권한 종류 수 + 예산 보유 1점을 세는 규칙 기반 추정치입니다.
- **결과**: 목표별 실측값 보고, 평가 기록과 결과 이력.
- **학습**: 최신 결과와 평가를 함께 보존하는 회고, 학습, 결정 담당자·기한 및 감사 타임라인.

책임자, 유효한 KPI와 충분한 권한 없이는 활성화할 수 없습니다. 권한 축소로 요건을 잃은 활성 목표는 초안으로 돌아갑니다. 책임자가 바뀌어도 다시 활성화해야 합니다. 빈 KPI/텍스트, 잘못된 지표 방향·숫자·날짜·예산, 중복 ID, 존재하지 않는 참조를 거부합니다. 리뷰에 학습도 결정도 없으면 컴파일러 경고를 남깁니다.

## API

JSON 입력과 출력에 `/api` 접두사를 사용합니다. 쓰기는 `Content-Type: application/json`이 필요합니다. 1MB 요청 제한, 기본 loopback 바인딩, 안전한 DOM 렌더링과 CSP를 적용합니다.

| Method | Path | Input / Output |
| --- | --- | --- |
| GET | `/api/organizations` | `[{organization:{id,name,createdAt},version,demo,peopleCount,goalCount}]` |
| POST | `/api/organizations` | `{"id":"team","name":"우리 조직"}` → 201 |
| GET | `/api/organizations/:oid` | 위 목록 항목과 같은 상세 객체 |
| PATCH | `/api/organizations/:oid` | `{"name":"새 이름","expectedVersion":52}` → 200 |
| DELETE | `/api/organizations/:oid` | `{"confirmName":"정확한 현재 이름","expectedVersion":52}` → 200 |
| POST | `/api/demo` | `{}` → 별도 데모 조직 원자 추가, 201 |
| GET | `/api/organizations/:oid/dashboard` | 선택한 조직의 전체 화면 projection |
| GET | `/api/organizations/:oid/people`, `/goals` | 해당 조직의 사람·목표 |
| GET | `/api/organizations/:oid/compiler`, `/graph`, `/events`, `/reviews` | 해당 조직 진단·관계·현재 수명주기 기록 |
| POST | `/api/organizations/:oid/people` | `{"id":"alice","name":"Alice","role":"Sales Lead"}` |
| POST | `/api/organizations/:oid/goals` | Goal JSON (아래 예시), organization이 :oid와 일치해야 함 |
| POST | `/api/organizations/:oid/goals/:id/owner` | `{"owner":"alice"}` |
| POST | `/api/organizations/:oid/people/:id/authority` | Authority JSON (아래 예시) |
| POST | `/api/organizations/:oid/goals/:id/authority` | 현재 목표 책임자에 대한 Authority JSON |
| POST | `/api/organizations/:oid/goals/:id/activate` | `{}` |
| POST | `/api/organizations/:oid/goals/:id/results` | `{"value":39,"reportedBy":"alice","note":"분기 매출"}` |
| POST | `/api/organizations/:oid/evaluations` | `{"goal":"revenue"}` |
| POST | `/api/organizations/:oid/reviews` | `{"id":"r1","goal":"revenue","note":"회고","learnings":[{"text":"배운 점"}],"decisions":[]}` |
| POST | `/api/organizations/:oid/goals/:id/strategy` | `{"note":"새 전략과 변경 이유"}` |

표에서 줄여 쓴 `/goals`, `/graph` 등의 경로에도 동일한 `/api/organizations/:oid` 접두사를 붙입니다. 기존 `/api/dashboard`, `/api/people` 등 조직 없는 경로는 **활성 조직이 정확히 하나일 때만** 호환합니다. 여러 조직이면 409와 명시적인 조직 경로 안내를 반환합니다. 조직이 하나도 없을 때 조회는 빈 projection을 반환하지만 구성원/목표 등 참조가 필요한 쓰기는 거절합니다. 서버가 임의의 기본 조직을 선택하지 않습니다.

`version`은 그 조직에 적용된 마지막 이벤트의 전역 순번입니다. 다른 조직을 수정해도 이 조직의 version은 바뀌지 않으므로 불필요한 충돌이 생기지 않습니다. 동일 조직이 변경되면 오래된 수정/삭제 요청은 409로 거절됩니다.

```json
{
  "id": "revenue", "organization": "team", "description": "매출 확대",
  "metric": {"id":"sales", "name":"매출", "unit":"억원", "direction":"HigherIsBetter"},
  "baseline": 30, "target": 50,
  "startsAt":"2026-09-01T00:00:00Z", "deadline":"2026-12-31T00:00:00Z",
  "requiredPermissions":["Pricing"], "requiredBudget":50000000
}
```

```json
{"owner":"alice","budgetLimit":50000000,"canHire":false,"canChangePrice":true,"canApprove":[]}
```

권한 목록은 Pricing, Hiring, BudgetApproval, Contracting, Marketing, Infrastructure, ProductLaunch입니다. 선택적인 `actor`는 대상 조직에 존재하는 구성원 ID여야 합니다. 사람·목표·지표·회고 ID는 서로 다른 조직에서 같아도 되며 타 조직 참조는 허용하지 않습니다. 서버가 이벤트 순번·시각, 결과 시각, 평가 계산값을 부여합니다. 없는 대상은 404, 잘못된 입력은 400, 중복·상태 충돌은 409, 저장 실패는 500으로 반환합니다.

## 저장과 제한

파일 저장은 프로세스 내 직렬화와 임시 파일 후 rename으로 처리하며 저장 성공 후에만 메모리 상태를 바꿉니다. 이벤트 파일이 손상되거나 순번이 어긋나면 시작을 거절합니다. 같은 파일을 여러 서버가 열지 못하도록 `.lock` 디렉터리를 사용합니다. 강제 종료로 잠금이 남으면 해당 서버가 종료되었는지 확인한 뒤 잠금 디렉터리를 수동 정리해야 합니다. 파일 교체는 원자적이지만 전원 장애에 대한 fsync 보장은 제공하지 않습니다. 이벤트 전체를 재생하는 작은 조직용 MVP이며 대규모 저장소용 snapshot/compaction은 아직 없습니다.

선택 SQLite 어댑터는 `sqlite-simple`을 사용합니다. `MY_ORG_SQLITE_FILE`에 로컬 DB 파일 경로를 지정한 경우에만 SQLite를 선택하며, 별도 DB 서버는 필요하지 않습니다. 미설정 시 기존 JSON 파일 저장을 그대로 사용합니다. DB의 부모 디렉터리는 자동 생성하며 빈 경로와 `:memory:`는 허용하지 않습니다.

```sh
# 기본 JSON 저장
make run

# 선택 SQLite 저장
MY_ORG_SQLITE_FILE=runs/local/events.sqlite3 make run
```

위 기본 실행 예시는 `MY_ORG_SQLITE_FILE`이 설정되지 않은 셸 기준입니다. 일반 모드에서 SQLite 설정은 `MY_ORG_EVENT_FILE`보다 우선하며, `MY_ORG_PORT`는 두 저장 방식 모두에 적용됩니다. 데모 모드는 SQLite 설정을 무시합니다.

SQLite는 `my_org_events` 테이블에 이벤트 순번과 기존 JSON payload를 보관하며 새 이벤트 묶음을 트랜잭션으로 추가합니다. 저장 성공 후에만 메모리 상태를 바꾸고, 손상된 이벤트나 불연속 순번은 시작 시 거절합니다. `synchronous=FULL`로 저장을 동기화하고 `locking_mode=EXCLUSIVE`와 `journal_mode=DELETE`를 사용하여 연결이 열린 동안 DB를 독점합니다. 같은 DB를 사용하는 두 번째 서버나 다른 DB 연결은 잠금 때문에 접근이 제한됩니다. 연결을 닫거나 프로세스가 종료되면 잠금이 풀리고, 강제 종료로 남은 미완료 트랜잭션은 SQLite rollback journal로 복구합니다. JSON 저장소의 `.lock` 디렉터리와 달리 SQLite에는 별도 잠금 디렉터리가 없습니다. 기존 WAL DB는 DELETE journal로 전환하며 다른 연결이 사용 중이면 시작에 실패할 수 있습니다. 저장 경로를 로그로 출력하지 않습니다.

기존 JSON 파일과 PostgreSQL 데이터는 SQLite로 자동 변환하지 않습니다. 새 SQLite 파일을 선택하면 별도 저장소로 시작하므로, 기존 JSON 데이터를 계속 사용하려면 `MY_ORG_SQLITE_FILE`을 설정하지 마세요. 이전 `MY_ORG_TEST_DATABASE_URL` 설정은 더 이상 사용하지 않습니다.

계정 인증·TLS·실제 ERP/CRM 권한 동기화·LLM은 구현하지 않았습니다. 감사 actor는 입력한 기록 주체이며 인증된 신원 증명이 아닙니다. 인터넷에 직접 공개하지 마세요. 조직의 권한 모델은 앱 접근 제어와 다릅니다. 활성 목표 타입은 비공개 생성자와 일반 getter를 사용하고, 외부 쓰기는 검증된 Command 경계로만 이벤트를 생성합니다. 도메인 replay는 신뢰하는 서버 생성 이벤트용입니다.

## 기능을 바로 체험하기

```sh
make demo
```

[데모 워크스페이스](http://127.0.0.1:8081)를 엽니다. 최초 실행은 **6명·7개 목표**와 결과, 평가, 회고, 학습, 전략 기록을 자동 생성합니다. 이 명령은 `MY_ORG_DEMO=1`을 지정하며 앱이 저장 위치를 `runs/demo/events.json`, 포트를 `8081`로 고정합니다. `MY_ORG_EVENT_FILE`·`MY_ORG_PORT`·`MY_ORG_SQLITE_FILE`의 기존 값을 사용하지 않습니다. 일반 `make run`의 `runs/local/events.json`과 분리됩니다.

재실행하면 기존 데모와 사용자가 바꾼 내용을 그대로 이어갑니다. 자동 초기화하거나 재시드하지 않습니다. 데모를 삭제한 뒤 다시 실행해도 자동으로 부활하지 않습니다. 삭제 후 새 일반 조직을 만든 경우에도 그대로 이어갑니다. 원래부터 데모 출처 없이 다른 조직이 들어 있던 데모 경로는 서버 시작을 거절하고 데이터를 보존합니다. `runs/`는 Git에서 제외됩니다.

일반 실행의 조직 목록에서도 **데모 추가**를 누를 수 있습니다. 다른 일반 조직이 있어도 별도 데모 조직을 추가하며 기존 조직은 바뀌지 않습니다. 별도 체험 파일을 원하면 `make demo`를 사용하세요. `POST /api/demo`에 `{}`를 보내면 같은 작업을 합니다. 성공은 201, 활성 데모 조직 ID가 이미 존재하면 409, 저장 실패는 500입니다. 데모를 삭제했다면 새 수명주기로 다시 만들 수 있습니다. 삭제 후 재시드는 과거 감사 기록 뒤에 새 전역 순번으로 추가됩니다. 여러 브라우저에서 동시에 요청해도 한 번만 생성됩니다. 모든 명령을 검증한 뒤 같은 Store 잠금 안에서 단 한 번 저장하므로 부분 시드는 남지 않습니다.

### 코드와 체험의 연결

| 모듈 | 구현된 역할 |
| --- | --- |
| `MyOrg.Application`, `Application.Command.Types` | 공개 명령 타입과 업무별 처리 함수로 연결하는 순수 디스패처 |
| `Application.Command.Organization`, `People`, `Goals`, `Authority`, `Review`, `Validation` | 업무별 명령 검증·이벤트 생성과 공통 입력·조회 검증 |
| `Domain.Identity`, `Organization`, `Goal.Types`, `Authority`, `Result`, `Review.Types`, `Error` | 식별자와 업무별 타입·권한 규칙·오류 값 |
| `Domain.Goal`, `State`, `Queries`, `Validation`, `Reducer` | 활성 목표 불변식, 상태 조회·검증, 권한 축소 시 초안 복귀와 이벤트 재생 |
| `Domain.Event.Types` | 도메인 이벤트와 저장 이벤트 봉투의 순수 타입 |
| `Evaluation`, `Compiler`, `Graph`, `Analysis` | KPI 평가, 구조 진단, 책임 그래프와 통제율 |
| `MyOrg.Demo` | 기준 시각을 받는 순수 시나리오, 기존 Command를 통한 검증된 이벤트 생성 |
| `MyOrg.Registry`, `Application.Plan` | 순수한 조직별 projection·범위 검증·명령/데모 이벤트 계획 |
| `Application.Runtime`, `Application.Persistence` | 저장 포트를 통해 잠금 안에서 계획·저장·메모리 반영을 원자적으로 조율 |
| `Infrastructure.FileStore`, `Infrastructure.SQLiteStore` | 파일/DB 자원 획득·해제와 영속 저장. `MyOrg.Store`는 조립 진입점 |
| `Application.Query`, `Application.ReadModel` | 명시적 조직 선택과 타입이 있는 조회 결과의 순수 계산 |
| `MyOrg.Server`, `Http.Route`, `Http.Encode` | 요청/시각/저장소 IO 조율, HTTP 경로·요청 파싱과 JSON 응답 변환 |
| `Serialization.Codec`, `Identity`, `Organization`, `Goal`, `Authority`, `Result`, `Review`, `Event` | 명시적으로 전달하는 `Codec a`와 업무 값·이벤트의 안정된 JSON 표현 |
| `Serialization.Persistence` | 파일·SQLite 어댑터가 사용하는 저장 이벤트 단건/목록 코덱 API |
| `Http.Codec`, `Http.Codec.*` | HTTP Wire 진입점과 분석·진단·오류·그래프·회고 표시 코덱 |
| `Serialization.JSON` | 기존 Haskell 호출자를 위한 `Http.Codec` 호환 파사드 |
| `Presentation.Analysis`, `Diagnostic`, `Review`, `Error`, `Event` | 구조화된 판단을 기존 분석·진단·회고·오류·감사 문구와 표시 모델로 변환 |
| `static/src/` | 다섯 화면, 실제 저장 상태로 판정하는 체험 가이드 |

기존 브라우저 데모의 여러 POST 호출은 한 번의 서버 시드 API로 대체했습니다. 초기 화면의 샘플 수를 늘리는 것과 함께 기존 기능을 조작할 이유와 다음 행동을 안내합니다.

명령의 저장 흐름은 `HTTP → Runtime → Plan → executeCommand`입니다. `Plan`은 주어진 시각과 기존 이벤트로 추가 이벤트를 계산합니다. `Runtime`은 같은 잠금 안에서 최신 이벤트를 읽고 계획한 뒤 저장 포트를 호출하며, 저장에 성공한 경우에만 메모리를 바꿉니다. 파일·DB 어댑터를 교체해도 명령 규칙은 바뀌지 않습니다. JSON 변환도 순수 계산이지만 외부 형식에 대한 책임이므로 도메인 규칙과는 별도 경계로 관리합니다.

조회는 `Http.Route → Application.Query → ReadModel → Http.Encode`로 이어집니다. Query는 URL 문자열이나 JSON `Value`를 받지 않고 조회 타입, 조직 레지스트리와 시각을 받습니다. HTTP 없이도 조직 범위·평가 시각·감사 순서를 테스트할 수 있습니다.

도메인 모듈은 Aeson 인스턴스를 갖지 않습니다. 공통 `Serialization.Codec`의 `Codec a`는 값 인코더와 디코더를 명시적으로 묶으며, 식별자·조직·목표·권한·결과·회고·이벤트의 코덱은 각각의 기능 모듈에 있습니다. HTTP와 저장 경계는 이 안정된 값 표현을 공유하되 진입점은 별도로 사용합니다.

파일·SQLite 어댑터는 `Serialization.Persistence`의 `encodeStoredEvent`/`decodeStoredEvent` 또는 목록용 `encodeStoredEvents`/`decodeStoredEvents`를 호출합니다. 이 저장 경계는 HTTP나 Presentation의 표시 코덱에 의존하지 않습니다. HTTP 경로·응답은 `Http.Codec`의 `toWire`/`parseWire`, `encodeWire`/`eitherDecodeWire`를 사용하며 `Http.Codec.Analysis`, `Diagnostic`, `Error`, `Graph`, `Review`가 표시 형식을 담당합니다. `Serialization.JSON`은 기존 Haskell 호출자를 위해 `Http.Codec`를 재노출하는 호환 파사드이고 신규 어댑터는 목적에 맞는 경계를 직접 가져옵니다.

기존 `MyOrg.Types`와 `Domain.Event`도 타입·함수 접근을 위한 호환 진입점이며 내부 코드는 필요한 세부 모듈을 직접 가져옵니다. 도메인 값을 직접 Aeson `encode`/`eitherDecode`에 전달하는 대신 해당 경계의 명시적 코덱을 사용하세요. HTTP 응답과 저장된 JSON의 키·태그·선택 필드 형식은 그대로 유지합니다.

분석과 진단은 도메인에서 구조화된 원인·자원·권고·대상 값을 계산합니다. `Presentation.Analysis`와 `Presentation.Diagnostic`이 이를 기존 문구와 표시 모델로 변환하고, 회고 경고 문구는 `Presentation.Review`가 담당합니다. HTTP의 `message`와 `possibleCause`는 기존처럼 문자열이며 표시 JSON을 읽을 때는 표시 모델을 사용합니다. 표시 문구로 도메인 판단을 다시 추론하지 않으며 저장 이벤트에는 이 진단 표현이 포함되지 않습니다.

명령은 `Application.Command.*`의 업무별 함수가 검증하고 이벤트를 만듭니다. 직원 프로필·보고 관계는 People에, 조직 수명주기는 Organization에 모으고, 공유 검증 함수는 직접 재사용합니다. `executeCommand`를 재호출하여 다른 명령의 검증 결과를 꺼내지 않습니다.

새 기능은 업무 규칙을 도메인에, 명령·조회 조합을 Application에, 외부 형식 변경을 Http/Serialization에, 실제 I/O를 Runtime/Infrastructure에 추가합니다. `npm run check`는 Elm 경계와 함께 순수 Haskell 모듈의 외부 의존 및 상태 계층의 역방향 의존을 검사합니다. 저장 코덱에서 HTTP·Presentation으로 이어지는 간접 의존과 HTTP에서 저장 구현으로 이어지는 의존도 검사합니다.

### 일곱 가지 시나리오

| 목표 | KPI와 최초 실측값 | 최초 상태 | 확인할 점 |
| --- | --- | --- | --- |
| 전사 성장 | 0 → 100점, 실측 없음 | 활성 · 결과 대기 | 나머지 목표의 부모. 지수는 자동 합계가 아닌 별도 입력 KPI |
| 엔터프라이즈 매출 | 30 → 50억원, 실측 42 | 활성 · 정상 | 34 → 38 → 42 가상 추이, 새 실측 50으로 달성 |
| 신규 고객 확보 | 1,000 → 5,000명, 실측 2,280 | 활성 · 위험 | 기대 진척 약 50% 대비 실제 32% |
| 서비스 안정성 | 10 → 0건, 실측 9 | 활성 · 이탈 | 낮아야 좋은 지표, 실제 진척 10% |
| 고객 유지 | 8 → 3%, 실측 3 | 활성 · 달성 | 낮아야 좋은 지표의 달성과 좋은 회고 |
| 신제품 출시 | 0 → 100% | 초안 · 권한 부족 | 제품 책임자 채용 권한·예산 부족 O017 |
| 파트너십 확장 | 0 → 10건 | 초안 · 미지정 | 최종 책임자 누락 O001 |

CEO에게 전체 권한 점수의 약 57%가 모여 O031, 고객 확보 회고에는 학습과 결정이 없어 O040이 나타납니다. 이는 의도한 체험 사례입니다. 모든 목표의 시작·마감은 최초 생성 기준 -50일·+50일, 회고 결정 기한은 +14일입니다. 시간이 지나면 기대 진척과 상태가 바뀔 수 있습니다. 데모를 재실행해도 날짜를 새로 쓰지 않습니다.

초기 결과 세 개씩은 **가상 연속 측정 샘플**입니다. 과거에 실제 측정한 것처럼 감사 시각을 소급하지 않으며 모두 실제 가져온 시각을 기록합니다. 최신 샘플이 현재 실측값이 됩니다. 결과 화면에서 목표별 결과 추이를 볼 수 있습니다.

### 네 단계 따라 하기

1. 가이드의 **01 · 빈 책임 자리 채우기**의 **이 단계 진행 →**를 누릅니다. 책임 화면의 파트너십 목표에서 `한유진 · 파트너십 책임자`를 지정합니다. 다시 안내를 따라 목표 화면의 해당 목표를 활성화합니다. 상태가 결과 대기로 바뀌면 첫 단계가 완료됩니다.
2. **02 · 책임에 맞는 권한 주기**의 **이 단계 진행 →**를 누릅니다. 이지원에게 `채용`을 추가하고 기존 `제품 출시` 권한을 유지하며 예산 한도를 `30000000`으로 저장합니다. 안내의 **이 단계 진행 →**를 누르고 신제품 출시 목표를 활성화합니다. 수정 전에 활성화를 시도하면 부족한 권한을 설명하는 오류를 볼 수 있습니다.
3. **03 · 결과에서 평가까지**의 **이 단계 진행 →**를 눌러 매출 실측값 `50`, 보고자 `김민서`, 결과 설명을 입력해 저장합니다. 상태가 달성이 된 뒤 같은 목표의 **평가 기록**도 누릅니다. 가이드는 달성 결과와 달성 평가 이벤트를 모두 확인해야 완료됩니다.
4. **04 · 배움을 다음 결정으로**의 **이 단계 진행 →**를 누르면 매출 목표가 선택됩니다. 회고 요약, 학습, 다음 결정, 담당자, 미래 기한을 입력합니다. 달성 결과·평가와 학습·담당자·기한 있는 결정이 저장되면 마지막 단계가 완료됩니다.

가이드를 접거나 다시 펼칠 수 있으며 다섯 화면 모두에서 이어집니다. 버튼을 클릭했다는 이유만으로 완료 처리하지 않습니다. 새로고침해도 실제 데이터에서 진행 상태를 다시 계산합니다. 데모 조직에서만 가이드가 나타납니다. 그래프와 감사 기록도 안내 버튼으로 바로 이동할 수 있습니다.


## 조직 수정·삭제와 다시 시작

조직 목록의 **상세 · 수정 · 삭제**를 누르면 현재 이름과 구성원/목표 수를 확인할 수 있습니다. 새 이름으로 **이름 저장**하면 ID·생성 시각·구성원·목표와 데모 출처는 보존됩니다.

**삭제 확인 열기…**를 누른 뒤 표시된 이름을 정확히 입력해야 **조직 삭제** 버튼이 활성화됩니다. **취소** 또는 Escape는 저장소를 변경하지 않습니다. 삭제 범위는 해당 조직의 구성원·목표·책임·권한·결과·평가·회고·전략이며 다른 조직은 유지됩니다. 삭제 후 목록으로 돌아가고 같은 ID로 새 조직 또는 데모를 만들 수 있습니다.

이는 **논리 삭제**입니다. 해당 조직 범위의 `OrganizationDeleted` 이벤트를 추가하고 그 조직의 현재 상태만 초기화합니다. JSON 파일이나 SQLite의 원본 감사 이벤트를 물리적으로 지우지 않습니다. 화면과 `/api/organizations/:oid/events`는 해당 조직의 최신 생성 이후 기록만 반환하므로 같은 ID로 재생성해도 이전 결과·회고·감사가 섞이지 않습니다. 과거 조직 조회나 영구 삭제 기능은 제공하지 않습니다.

이름 수정 초안에는 입력을 시작한 조직 버전을 보관하고, 삭제 확인에는 대상 ID·이름·version을 고정합니다. 동일 조직이 다른 탭에서 변경되면 **409**로 거절하고 최신 상태를 조회해 다시 확인하도록 합니다. 자동 재시도하지 않습니다. 조직을 전환하면 기존 확인 snapshot을 폐기합니다. 브라우저 요청도 시작한 조직을 고정하여 늦은 A 응답이 B 화면을 덮어쓰지 않게 처리합니다.

`make demo`는 최초 완전히 빈 데모 저장소만 자동 시드합니다. 기존 데모 저장소의 여러 조직, 삭제 상태, 새 일반 조직을 재시작 때 보존합니다. 삭제한 데모를 자동으로 부활시키지 않습니다. 원래부터 데모 출처 없는 일반 조직만 들어 있던 데모 경로는 비수정 상태로 시작을 거절합니다.

## 기존 데이터 호환

이전 `StoredEvent` JSON과 생성자 형태를 유지합니다. 신규 저장 이벤트만 `OrganizationScoped OrgId OrganizationEvent`로 감싸 조직 범위를 기록하며 읽기만으로 기존 파일을 마이그레이션하거나 재작성하지 않습니다. 구형 unscoped 이벤트는 가장 최근의 구형 OrganizationCreated가 연 조직에 귀속하고, 구형 삭제가 그 구간을 닫습니다. 새 scoped 이벤트는 이 해석 커서를 바꾸지 않으므로 혼합 로그도 안정적으로 재생됩니다.

저장소 전체 이벤트 순번은 연속 증가합니다. Registry가 한 단계 unwrap한 조직별 스트림으로 기존 단일 조직 엔진을 재사용하며, 중첩 wrapper·이벤트 내부 조직과 범위의 불일치·연속되지 않는 순번은 거절합니다. 단순히 데모와 같은 조직 ID를 사용했다고 데모 가이드가 나오지 않습니다. 조직별 현재 스트림의 DemoSeeded 출처를 사용하고, 이전 v2 데모는 원래 51개 시드 이벤트 내용 전체가 일치할 때만 호환 인식합니다.
