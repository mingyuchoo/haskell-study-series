# 다국어 처리 구현 완료

## 개요

TUI Todo 애플리케이션에 영어와 한국어 다국어 지원을 MTL 아키텍처에 맞게 구현했습니다.

## 구현된 파일

### 1. 다국어 메시지 파일 (config/)
- `config/messages-ko.yaml` - 한국어 메시지
- `config/messages-en.yaml` - 영어 메시지
- `config/README_I18N.md` - 다국어 사용 가이드

### 2. I18n 모듈 (src/I18n.hs)
새로 생성된 모듈로 다음 기능 제공:
- `Language` 타입: English, Korean 지원
- `I18nMessages` 및 관련 데이터 타입들:
  - `UIMessages`: UI 텍스트 (헤더, 제목 등)
  - `FieldLabels`: 필드 레이블
  - `StatusMessages`: 상태 메시지
  - `ListMessages`: 리스트 표시 메시지
  - `HelpMessages`: 도움말 메시지
  - `SystemMessages`: 시스템 메시지
  - `SampleTodos`: 샘플 할일 메시지
- `loadMessages`: YAML 파일에서 메시지 로드
- `defaultMessages`: 기본 영어 메시지

### 3. App 모듈 수정 (src/App.hs)
MTL 패턴에 맞게 수정:
- `AppEnv`에 `envMessages :: I18nMessages` 필드 추가
- `MonadApp` 타입클래스에 `getMessages` 함수 추가
- 애플리케이션 전체에서 다국어 메시지 접근 가능

### 4. Config 모듈 수정 (src/Config.hs)
- `loadKeyBindingsWithMessages` 함수 추가
- 키바인딩 로드 시 다국어 메시지 사용

### 5. DB 모듈 수정 (src/DB.hs)
- `initDBWithMessages` 함수 추가
- 샘플 할일 생성 시 다국어 메시지 사용

### 6. Lib 모듈 수정 (src/Lib.hs)
모든 하드코딩된 문자열을 다국어 처리:
- `AppState`에 `_i18nMessages` 필드 추가
- `drawHeader`: 헤더 텍스트
- `drawTodoList`: 할일 목록 UI
- `drawTodo`: 개별 할일 항목 표시
- `drawDetailView`: 상세 정보 (보기/편집/추가 모드)
- `drawHelp`: 도움말 메시지
- `tuiMain`: 기본 언어 설정 (한국어)
- `tuiMainWithLanguage`: 언어 선택 가능한 진입점

## 다국어 처리된 텍스트

### UI 요소
- 헤더: "📝 할일 관리자" / "📝 Todo Manager"
- 제목들: "할일 목록", "상세 정보" 등
- 빈 상태 메시지: "할일이 없습니다..." 등

### 필드 레이블
- ID, 상태, 할일, 주체자, 대상자, 작업대상
- 생성 시각, 완료 시각

### 상태 메시지
- "✓ 완료됨" / "✓ Completed"
- "○ 진행중" / "○ In Progress"

### 리스트 표시
- 체크박스: "[✓] " / "[ ] "
- 필드 구분자: " | "
- 시간 접두사: "완료: ", "생성: "

### 도움말
- 각 모드별 키 안내 메시지

### 시스템 메시지
- 설정 파일 로드 메시지
- 오류 메시지

### 샘플 할일
- 환영 메시지
- 사용법 힌트

## 사용 방법

### 기본 사용 (한국어)
```bash
stack build
stack exec tui-todo-exe
```

### 영어로 변경
`src/Lib.hs` 수정:
```haskell
tuiMain :: IO ()
tuiMain = tuiMainWithLanguage I18n.English
```

그 후 재빌드:
```bash
stack build
stack exec tui-todo-exe
```

## MTL 아키텍처 준수

1. **환경 분리**: `AppEnv`에 의존성 주입
2. **타입클래스 기반**: `MonadApp`으로 추상화
3. **순수 함수**: 메시지 로딩은 IO에서만 수행
4. **불변성**: 메시지는 초기화 시 한 번만 로드
5. **테스트 가능**: 다른 언어로 쉽게 테스트 가능

## 확장 가능성

새 언어 추가 시:
1. `src/I18n.hs`의 `Language` 타입에 추가
2. `config/messages-XX.yaml` 파일 생성
3. `loadMessages` 함수에 경로 추가

모든 UI 텍스트가 중앙화되어 있어 유지보수가 용이합니다.

## 테스트 결과

✅ 빌드 성공
✅ 한국어 UI 정상 작동
✅ 모든 텍스트 다국어 처리 완료
✅ MTL 패턴 준수
