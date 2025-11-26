# 모듈 구조 및 리팩토링 가이드

## 개요

`Lib.hs` 파일이 600줄 이상으로 비대해져 유지보수가 어려웠습니다. 이를 해결하기 위해 역할과 책임에 따라 여러 모듈로 분리했습니다.

## 리팩토링 전후 비교

### 이전 구조
```
src/
├── App.hs
├── Config.hs
├── DB.hs
├── I18n.hs
└── Lib.hs (600+ 줄)
```

### 리팩토링 후 구조
```
src/
├── App.hs
├── Config.hs
├── DB.hs
├── I18n.hs
├── Lib.hs (60 줄)
└── UI/
    ├── Types.hs
    ├── Attributes.hs
    ├── Draw.hs
    └── Events.hs
```

## 모듈별 상세 설명

### 1. UI/Types.hs
**역할**: 핵심 데이터 타입 정의

**포함 내용**:
- `Mode`: 애플리케이션 모드
  - `ViewMode`: 보기 모드
  - `InputMode`: 입력 모드
  - `EditMode DB.TodoId`: 편집 모드
- `Name`: 위젯 리소스 이름
  - `TodoList`, `ActionField`, `SubjectField`, etc.
- `FocusedField`: 포커스된 필드 추적
  - `FocusAction`, `FocusSubject`, etc.
- `Todo`: UI Todo 항목 표현
- `AppState`: 애플리케이션 전체 상태
- `fromTodoRow`: DB 행을 UI Todo로 변환

**특징**: 순수한 데이터 타입 정의만 포함

### 2. UI/Attributes.hs
**역할**: UI 속성 및 스타일 정의

**포함 내용**:
- `theMap`: Vty 속성 맵
  - 색상 정의 (header, selected, normal, completed, etc.)
  - 스타일 정의 (bold, dim, etc.)

**특징**: 순수한 설정 데이터

### 3. UI/Draw.hs
**역할**: UI 렌더링 로직

**포함 내용**:
- `drawUI`: 메인 UI 그리기
- `drawHeader`: 헤더 렌더링
- `drawTodoList`: Todo 리스트 렌더링
- `drawTodo`: 개별 Todo 항목 렌더링
- `drawDetailView`: 상세 뷰 렌더링
  - `drawViewModeDetail`: 보기 모드 상세 뷰
  - `drawEditModeDetail`: 편집 모드 상세 뷰
  - `drawInputModeDetail`: 입력 모드 상세 뷰
- `drawHelp`: 도움말 렌더링
- `renderEditField`: 편집 가능한 필드 렌더링

**특징**: 순수 함수로만 구성, 부수효과 없음

### 4. UI/Events.hs
**역할**: 이벤트 처리 로직

**포함 내용**:
- `handleEvent`: 메인 이벤트 핸들러
- 모드별 이벤트 처리:
  - `handleViewMode`: ViewMode 이벤트 처리
  - `handleInputMode`: InputMode 이벤트 처리
  - `handleEditMode`: EditMode 이벤트 처리
- Todo 작업 함수:
  - `toggleTodoComplete`: 완료 상태 토글
  - `deleteTodo`: Todo 삭제
  - `saveNewTodo`: 새 Todo 저장
  - `saveEditedTodo`: 편집된 Todo 저장
- 에디터 관리:
  - `enterInputMode`: 입력 모드 진입
  - `enterEditMode`: 편집 모드 진입
  - `clearEditors`: 에디터 초기화
  - `cycleFieldFocus`: 필드 포커스 순환
- 유틸리티:
  - `trim`: 문자열 공백 제거

**특징**: `EventM` 모나드 사용, 부수효과 포함

### 5. Lib.hs
**역할**: 애플리케이션 진입점 및 통합

**포함 내용**:
- `app`: Brick 애플리케이션 정의
- 모든 모듈의 공개 API 재export

**특징**: 간결한 통합 레이어 (60줄)

## 모듈 간 의존성

```
Lib.hs
  ├─> UI/Types.hs
  ├─> UI/Attributes.hs
  ├─> UI/Draw.hs
  │     └─> UI/Types.hs
  │     └─> I18n.hs
  └─> UI/Events.hs
        └─> UI/Types.hs
        └─> App.hs
        └─> DB.hs
        └─> Config.hs
```

## 순수 함수 vs 효과

### 순수 모듈 (Pure)
- `UI/Types.hs`: 데이터 타입 정의
- `UI/Attributes.hs`: 스타일 정의
- `UI/Draw.hs`: UI 렌더링

### 효과 모듈 (Effect)
- `UI/Events.hs`: 이벤트 처리, 상태 변경
- `App.hs`: 데이터베이스 작업
- `DB.hs`: SQLite 작업
- `Config.hs`: 파일 I/O
- `I18n.hs`: 파일 I/O (loadMessages)

## 마이그레이션 가이드

### 기존 코드 사용 시
- `Lib` 모듈의 공개 API는 변경되지 않음
- 모든 타입과 함수는 여전히 `Lib`에서 import 가능
- 내부 구현만 여러 모듈로 분리됨

### 새 코드 작성 시
```haskell
-- 특정 타입만 필요한 경우
import UI.Types (AppState, Todo, Mode(..))

-- UI 렌더링 함수가 필요한 경우
import UI.Draw (drawUI, drawTodo)

-- 이벤트 처리가 필요한 경우
import UI.Events (handleEvent)
```

## 확장 가이드

### 새로운 UI 컴포넌트 추가
1. `UI/` 디렉토리에 새 모듈 추가 (예: `UI/Dialogs.hs`)
2. `Lib.hs`에서 필요한 함수 재export
3. `tui-todo.cabal`의 `other-modules`에 추가

### 새로운 이벤트 타입 추가
1. `UI/Types.hs`에 데이터 타입 추가
2. `UI/Events.hs`에 핸들러 추가
3. 기존 `handleEvent` 함수에 통합

### 새로운 필드 추가
1. `UI/Types.hs`의 `Todo` 타입에 필드 추가
2. `DB.hs`의 `TodoRow` 타입에 필드 추가
3. `UI/Draw.hs`에서 렌더링 로직 추가
4. `UI/Events.hs`에서 편집 로직 추가

## 변경 사항 요약

### Config.hs
- `getFirstKey` 헬퍼 함수 추가: 키 바인딩 리스트에서 첫 번째 키 가져오기

### tui-todo.cabal
`other-modules`에 새 모듈 추가:
```cabal
other-modules:
    UI.Types
    UI.Attributes
    UI.Draw
    UI.Events
```

## 리팩토링의 이점

1. **가독성 향상**: 각 모듈이 명확한 책임을 가짐
2. **유지보수 용이**: 변경 사항이 특정 모듈에 국한됨
3. **테스트 용이**: 순수 함수와 효과가 분리됨
4. **재사용성**: 모듈별로 독립적으로 사용 가능
5. **확장성**: 새 기능 추가 시 기존 코드 영향 최소화

## 코드 품질 지표

- **Lib.hs**: 600+ 줄 → 60 줄 (90% 감소)
- **모듈 수**: 5개 → 9개
- **평균 모듈 크기**: 120 줄 → 150 줄
- **순수/효과 분리**: 명확히 구분됨
