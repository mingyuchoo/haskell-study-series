# 모듈 리팩토링 문서

## 개요

`Lib.hs` 파일이 너무 길고 복잡하여 유지보수가 어려웠습니다. 이를 해결하기 위해 역할과 책임에 따라 여러 모듈로 분리했습니다.

## 리팩토링 구조

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
├── Types.hs
└── UI/
    ├── Attributes.hs
    ├── Draw.hs
    └── Events.hs
```

## 모듈 설명

### 1. Types.hs
**역할**: 핵심 데이터 타입 정의
- `Mode`: 애플리케이션 모드 (ViewMode, InputMode, EditMode)
- `Name`: 위젯 리소스 이름
- `FocusedField`: 포커스된 필드 추적
- `Todo`: UI Todo 항목 표현
- `AppState`: 애플리케이션 상태
- `fromTodoRow`: DB 행을 UI Todo로 변환

### 2. UI/Draw.hs
**역할**: UI 렌더링 로직
- `drawUI`: 메인 UI 그리기
- `drawHeader`: 헤더 렌더링
- `drawTodoList`: Todo 리스트 렌더링
- `drawTodo`: 개별 Todo 항목 렌더링
- `drawDetailView`: 상세 뷰 렌더링
- `drawHelp`: 도움말 렌더링
- 모드별 상세 뷰 렌더링 함수들

### 3. UI/Events.hs
**역할**: 이벤트 처리 로직
- `handleEvent`: 메인 이벤트 핸들러
- `handleViewMode`: ViewMode 이벤트 처리
- `handleInputMode`: InputMode 이벤트 처리
- `handleEditMode`: EditMode 이벤트 처리
- Todo CRUD 작업 함수들
- 에디터 관리 함수들

### 4. UI/Attributes.hs
**역할**: UI 속성 및 스타일 정의
- `theMap`: 색상 및 스타일 속성 맵

### 5. Lib.hs (리팩토링됨)
**역할**: 메인 애플리케이션 정의 및 모듈 재내보내기
- `app`: Brick 애플리케이션 정의
- 다른 모듈의 타입 및 함수 재내보내기

## 개선 사항

### 1. 가독성 향상
- 각 모듈이 명확한 단일 책임을 가짐
- 함수 이름과 위치로 기능을 쉽게 파악 가능

### 2. 유지보수성 향상
- UI 렌더링 수정 시 `UI/Draw.hs`만 수정
- 이벤트 처리 수정 시 `UI/Events.hs`만 수정
- 타입 변경 시 `Types.hs`만 수정

### 3. 테스트 용이성
- 각 모듈을 독립적으로 테스트 가능
- 의존성이 명확하게 분리됨

### 4. 확장성
- 새로운 UI 컴포넌트 추가 시 `UI/` 디렉토리에 새 모듈 추가
- 새로운 이벤트 타입 추가 시 `UI/Events.hs`에 핸들러 추가

## 변경 사항

### Config.hs
- `getFirstKey` 헬퍼 함수 추가: 키 바인딩 리스트에서 첫 번째 키 가져오기

### tui-todo.cabal
- `other-modules`에 새 모듈 추가:
  - Types
  - UI.Attributes
  - UI.Draw
  - UI.Events

## 마이그레이션 가이드

기존 코드를 사용하는 경우:
- `Lib` 모듈의 공개 API는 변경되지 않음
- 모든 타입과 함수는 여전히 `Lib`에서 import 가능
- 내부 구현만 여러 모듈로 분리됨

새 코드 작성 시:
- 특정 타입만 필요한 경우 `Types` 직접 import
- UI 렌더링 함수가 필요한 경우 `UI.Draw` import
- 이벤트 핸들러가 필요한 경우 `UI.Events` import
