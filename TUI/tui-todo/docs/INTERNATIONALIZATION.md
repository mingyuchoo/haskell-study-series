# 다국어 지원 (Internationalization)

이 애플리케이션은 영어와 한국어를 지원합니다.

## 지원 언어

- 영어 (English): `config/messages-en.yaml`
- 한국어 (Korean): `config/messages-ko.yaml`

## 언어 설정

기본 언어는 한국어입니다. 언어를 변경하려면 `src/Lib.hs`의 `tuiMain` 함수를 수정하세요:

```haskell
-- 한국어 사용 (기본값)
tuiMain :: IO ()
tuiMain = tuiMainWithLanguage I18n.Korean

-- 영어 사용
tuiMain :: IO ()
tuiMain = tuiMainWithLanguage I18n.English
```

## 메시지 파일 구조

각 언어 파일은 다음과 같은 구조를 가집니다:

```yaml
language: ko  # 또는 en

ui:
  header: "헤더 텍스트"
  todos_title: "할일 목록 제목"
  # ... 기타 UI 메시지

fields:
  id_label: "ID 레이블"
  status_label: "상태 레이블"
  # ... 기타 필드 레이블

status:
  completed: "완료 상태 텍스트"
  in_progress: "진행중 상태 텍스트"

list:
  checkbox_done: "완료 체크박스"
  checkbox_todo: "미완료 체크박스"
  # ... 기타 리스트 표시 메시지

help:
  view_mode: "보기 모드 도움말"
  edit_mode: "편집 모드 도움말"
  input_mode: "입력 모드 도움말"

messages:
  config_not_found: "설정 파일 없음 메시지"
  # ... 기타 시스템 메시지

sample_todos:
  welcome: "환영 메시지"
  add_hint: "추가 힌트"
  toggle_hint: "토글 힌트"
```

## 새 언어 추가하기

1. `src/I18n.hs`의 `Language` 타입에 새 언어 추가:
   ```haskell
   data Language = English | Korean | Japanese
   ```

2. `config/messages-ja.yaml` 파일 생성 (일본어 예시)

3. `loadMessages` 함수에 새 언어 경로 추가:
   ```haskell
   let path = case lang of
           English  -> "config/messages-en.yaml"
           Korean   -> "config/messages-ko.yaml"
           Japanese -> "config/messages-ja.yaml"
   ```

## MTL 아키텍처

다국어 기능은 MTL (Monad Transformer Library) 패턴을 따릅니다:

- `I18n` 모듈: 다국어 메시지 데이터 타입과 로딩 함수
- `App.AppEnv`: 애플리케이션 환경에 `I18nMessages` 포함
- `App.MonadApp`: `getMessages` 함수로 메시지 접근
- `Lib.AppState`: UI 상태에 `_i18nMessages` 필드 포함

이를 통해 애플리케이션 전체에서 일관되게 다국어 메시지에 접근할 수 있습니다.
