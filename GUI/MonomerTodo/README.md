# Monomer Todo Application with SQLite

MTL 스타일로 리팩토링된 Monomer Todo 애플리케이션입니다. SQLite 데이터베이스를 사용하여 할일 목록을 영구 저장합니다.

## 주요 특징

- **MTL 스타일 아키텍처**: `MonadTodoRepo` 타입클래스를 사용한 추상화
- **SQLite 영구 저장**: 할일 목록이 `todos.db` 파일에 저장됨
- **관심사 분리**:
  - `TodoRepo.hs`: Repository 인터페이스 정의
  - `TodoRepoSqlite.hs`: SQLite 구현
  - `TodoLogic.hs`: 비즈니스 로직
  - `TodoTypes.hs`: 데이터 타입
  - `Main.hs`: UI 및 이벤트 처리

## 프로젝트 구조

```
src/
├── Main.hs              # UI 및 이벤트 핸들러
├── TodoTypes.hs         # 데이터 타입 정의
├── TodoRepo.hs          # Repository 인터페이스 (MTL)
├── TodoRepoSqlite.hs    # SQLite 구현
└── TodoLogic.hs         # 비즈니스 로직
```

## Prerequisites

### 시스템 의존성 설치

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install \
     libglew-dev \
     libsdl2-dev \
     libsdl2-ttf-dev \
     libsdl2-image-dev \
     pkg-config

# macOS
brew install pkg-config glfw3 sdl2 glew

# Fedora
sudo dnf install gcc-c++ SDL2-devel glew-devel

# Windows 11
stack setup
stack exec -- pacman -S msys2-keyring
stack exec -- pacman -S mingw-w64-x86_64-pkg-config
stack exec -- pacman -S mingw-w64-x86_64-SDL2
stack exec -- pacman -S mingw-w64-x86_64-freeglut
stack exec -- pacman -S mingw-w64-x86_64-glew
stack exec -- pacman -S mingw-w64-x86_64-freetype
stack exec -- pacman -Syu
```

## 빌드 및 실행

```bash
# 빌드
stack build

# 실행
stack exec app

# 또는 cabal 사용
cabal build
cabal run app
```

## 아키텍처 설명

### MTL 스타일 Repository 패턴

```haskell
-- 추상 인터페이스
class Monad m => MonadTodoRepo m where
  getAllTodos :: m [Todo]
  insertTodo :: Todo -> m Todo
  updateTodo :: Int -> Todo -> m ()
  deleteTodo :: Int -> m ()
  initializeDb :: m ()

-- SQLite 구현
type AppM = ReaderT SqliteEnv IO

instance MonadTodoRepo AppM where
  -- SQLite 구체적 구현
```

### 장점

1. **테스트 용이성**: Mock 구현으로 쉽게 테스트 가능
2. **유연성**: 다른 데이터베이스로 쉽게 전환 가능
3. **타입 안전성**: 컴파일 타임에 타입 체크
4. **관심사 분리**: UI, 로직, 데이터 레이어 명확히 분리

## 데이터베이스 스키마

```sql
CREATE TABLE todos (
  id INTEGER PRIMARY KEY,
  type INTEGER NOT NULL,      -- 0: Home, 1: Work, 2: Sports
  status INTEGER NOT NULL,    -- 0: Pending, 1: Done
  description TEXT NOT NULL
);
```

## 사용 방법

1. 애플리케이션 실행 시 `todos.db` 파일이 자동 생성됩니다
2. 데이터베이스가 비어있으면 샘플 데이터가 자동으로 추가됩니다
3. 모든 변경사항은 자동으로 데이터베이스에 저장됩니다
4. 애플리케이션을 재시작해도 데이터가 유지됩니다

## 참고 자료

- [Monomer Documentation](https://github.com/fjvallarino/monomer)
- [MTL Style Guide](https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/)
- [SQLite Simple](https://hackage.haskell.org/package/sqlite-simple)
