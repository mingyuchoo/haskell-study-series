# 다국어 지원 가이드

## 개요

TUI Todo 애플리케이션은 영어와 한국어를 지원하며, MTL 아키텍처에 맞게 구현되었습니다.

## 지원 언어

- **영어 (English)**: `config/messages-en.yaml`
- **한국어 (Korean)**: `config/messages-ko.yaml`

## 언어 설정 방법

### 기본 언어 변경

`app/Main.hs` 또는 `src/Lib.hs`에서 언어를 설정할 수 있습니다:

```haskell
-- 한국어 사용 (기본값)
app :: IO ()
app = appWithLanguage I18n.Korean

-- 영어 사용
app :: IO ()
app = appWithLanguage I18n.English
```

변경 후 재빌드:
```bash
stack build
stack exec tui-todo-exe
```

## 메시지 파일 구조

각 언어 파일(`config/messages-XX.yaml`)은 다음 구조를 따릅니다:

```yaml
language: ko  # 또는 en

ui:
  header: "📝 할일 관리자"
  todos_title: " 할일 목록 "
  detail_title: " 상세 정보 "
  detail_edit_title: " 상세 정보 (편집 모드) "
  detail_add_title: " 새 할일 추가 "
  no_todos: "할일이 없습니다. 'a'를 눌러 추가하세요!"
  no_selection: "선택된 할일이 없습니다"
  not_found: "편집할 항목을 찾을 수 없습니다"

fields:
  id_label: "ID"
  status_label: "상태"
  action_label: "할일"
  action_required_label: "할일 (필수)"
  subject_label: "주체자"
  indirect_object_label: "대상자"
  direct_object_label: "작업대상"
  created_at_label: "생성 시각"
  completed_at_label: "완료 시각"
  auto_generated_label: "(자동 생성)"

status:
  completed: "✓ 완료됨"
  in_progress: "○ 진행중"

list:
  checkbox_done: "[✓] "
  checkbox_todo: "[ ] "
  field_separator: " | "
  field_action: "할일"
  field_subject: "주체자"
  field_indirect: "대상자"
  field_direct: "작업대상"
  completed_prefix: "완료: "
  created_prefix: "생성: "

help:
  view_mode: "보기 모드 도움말"
  edit_mode: "편집 모드 도움말"
  input_mode: "입력 모드 도움말"
  add: "추가"
  edit: "편집"
  toggle: "토글"
  delete: "삭제"
  navigate: "이동"
  quit: "종료"

messages:
  config_not_found: "설정 파일을 찾을 수 없습니다"
  config_load_failed: "키바인딩 설정 로드 실패"
  config_loaded: "키바인딩 설정을 불러왔습니다."
  using_default: "기본 키바인딩을 사용합니다."
  i18n_not_found: "다국어 파일을 찾을 수 없습니다"
  i18n_load_failed: "다국어 파일 로드 실패"
  i18n_loaded: "다국어 설정을 불러왔습니다."
  using_default_lang: "기본 언어(영어)를 사용합니다."

sample_todos:
  welcome: "할일 관리자에 오신 것을 환영합니다!"
  add_hint: "'a'를 눌러 새 할일을 추가하세요"
  toggle_hint: "스페이스를 눌러 완료 상태를 변경하세요"
```

## 다국어 처리된 UI 요소

### 헤더 및 제목
- 애플리케이션 헤더
- 할일 목록 제목
- 상세 정보 제목 (보기/편집/추가 모드별)

### 필드 레이블
- ID, 상태, 할일
- 주체자, 대상자, 작업대상
- 생성 시각, 완료 시각

### 상태 메시지
- 완료됨 / 진행중
- 체크박스 표시

### 리스트 표시
- 필드 구분자
- 시간 접두사 (완료, 생성)

### 도움말
- 각 모드별 키 안내 메시지

### 시스템 메시지
- 설정 파일 로드 메시지
- 오류 메시지

### 샘플 할일
- 환영 메시지
- 사용법 힌트

## 구현 세부사항

### I18n 모듈 (src/I18n.hs)

#### 데이터 타입
```haskell
data Language = English | Korean

data I18nMessages = I18nMessages 
  { language     :: !String
  , ui           :: !UIMessages
  , fields       :: !FieldLabels
  , status       :: !StatusMessages
  , list         :: !ListMessages
  , help         :: !HelpMessages
  , messages     :: !SystemMessages
  , sample_todos :: !SampleTodos
  }
```

#### 메시지 로딩
```haskell
loadMessages :: Language -> IO I18nMessages
```

YAML 파일에서 메시지를 로드하며, 파일이 없거나 오류가 발생하면 기본 영어 메시지를 사용합니다.

### App 모듈 통합 (src/App.hs)

#### AppEnv에 메시지 추가
```haskell
data AppEnv = AppEnv 
  { envConnection :: !Connection
  , envMessages   :: !I18n.I18nMessages
  }
```

#### MonadApp 타입클래스
```haskell
class (MonadIO m, MonadReader AppEnv m) => MonadApp m where
    getConnection :: m Connection
    getMessages :: m I18n.I18nMessages
```

### UI 모듈에서 사용

#### UI/Draw.hs
```haskell
drawHeader :: AppState -> Widget Name
drawHeader s =
  withAttr (attrName "header") $
    hCenter $
      padTopBottom 1 $
        str $
          I18n.header (I18n.ui (s ^. i18nMessages))
```

모든 UI 텍스트가 `I18nMessages`에서 가져옵니다.

## 새 언어 추가하기

### 1단계: Language 타입 확장

`src/I18n.hs` 수정:
```haskell
data Language = English | Korean | Japanese
     deriving (Eq, Generic, Show)
```

### 2단계: 메시지 파일 생성

`config/messages-ja.yaml` 파일 생성 (일본어 예시):
```yaml
language: ja

ui:
  header: "📝 Todoマネージャー"
  todos_title: " Todoリスト "
  # ... 나머지 메시지
```

### 3단계: loadMessages 함수 수정

`src/I18n.hs`의 `loadMessages` 함수에 경로 추가:
```haskell
loadMessages :: Language -> IO I18nMessages
loadMessages lang = do
  let path = case lang of
        English  -> "config/messages-en.yaml"
        Korean   -> "config/messages-ko.yaml"
        Japanese -> "config/messages-ja.yaml"
  -- ... 나머지 로직
```

### 4단계: 빌드 및 테스트

```bash
stack build
stack exec tui-todo-exe
```

## MTL 아키텍처 준수

다국어 기능은 다음 원칙을 따릅니다:

1. **환경 분리**: `AppEnv`에 의존성 주입
2. **타입클래스 기반**: `MonadApp`으로 추상화
3. **순수 함수**: 메시지 데이터 타입은 순수
4. **불변성**: 메시지는 초기화 시 한 번만 로드
5. **테스트 가능**: 다른 언어로 쉽게 테스트 가능

## 테스트 결과

✅ 빌드 성공  
✅ 한국어 UI 정상 작동  
✅ 영어 UI 정상 작동  
✅ 모든 텍스트 다국어 처리 완료  
✅ MTL 패턴 준수  

## 유지보수 가이드

### 새 UI 텍스트 추가 시

1. 해당 데이터 타입에 필드 추가 (예: `UIMessages`)
2. 모든 언어 파일에 번역 추가
3. `defaultMessages`에 기본값 추가
4. UI 코드에서 사용

### 번역 수정 시

해당 언어의 YAML 파일만 수정하면 됩니다. 재빌드 필요 없음.

## 장점

- **중앙화**: 모든 UI 텍스트가 한 곳에서 관리됨
- **타입 안전**: 컴파일 타임에 메시지 필드 검증
- **확장 용이**: 새 언어 추가가 간단함
- **유지보수 용이**: 번역 수정이 쉬움
- **일관성**: 전체 애플리케이션에서 동일한 메시지 사용
