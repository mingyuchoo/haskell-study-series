# tto

## How to create a project

```bash
stack new <project-name> mingyuchoo/cli
```

## How to build

```bash
stack build
# or
stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"
```

## How to test as watch mode

```bash
stack test --fast --file-watch --watch-all
# or
stack test --coverage --fast --file-watch --watch-all --haddock
# or
ghcid --command "stack ghci test/Spec.hs"
```

## How to run

```bash
stack run
```
You can also use `Makefile` for these works.

## 키바인딩 설정

TUI Todo Manager는 사용자 정의 키바인딩을 지원합니다.

### 설정 파일

키바인딩 설정은 `config/keybindings.yaml` 파일에서 관리됩니다.

### 기본 키바인딩

- `q` 또는 `Esc`: 애플리케이션 종료
- `a`: 새 할 일 추가
- `Space`: 할 일 완료 상태 토글
- `d`: 할 일 삭제
- `↑` 또는 `k`: 위로 이동
- `↓` 또는 `j`: 아래로 이동

### 키바인딩 커스터마이징

`config/keybindings.yaml` 파일을 수정하여 원하는 키를 설정할 수 있습니다.

예시 - Vim 스타일 키바인딩:
```yaml
keybindings:
  quit: ['q']
  add_todo: ['i', 'a']
  toggle_complete: ['Space', 'x']
  delete_todo: ['d']
  navigate_up: ['k', 'Up']
  navigate_down: ['j', 'Down']
  save_input: ['Enter']
  cancel_input: ['Esc']
```

자세한 설정 방법은 `config/README.md`를 참조하세요.

### 예시 설정 파일

- `config/keybindings.yaml`: 기본 키바인딩
- `config/keybindings-vim.yaml`: Vim 스타일 키바인딩 예시
