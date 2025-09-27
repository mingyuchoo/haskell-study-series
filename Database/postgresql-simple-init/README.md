# postgresql-simple-init

## Prerequisite

### In PostgreSQL

```sql
CREATE TABLE test (
       id int
       , name varchar
);

DROP TABLE test;
```

### In Ubuntu

```bash
sudo apt install -y libpq-dev
```

### In Manjaro

```bash
sudo pacman -S postgresql-libs
```

### Add a package dependency

```yaml
dependencies:
- postgresql-simple
```

## Project structure

```text
.
├── CHANGELOG.md
├── Dockerfile
├── LICENSE
├── Makefile
├── README.md
├── Setup.hs
├── app/
│   └── Main.hs
├── docker-compose.yaml
├── format.sh
├── package.yaml
├── postgresql-simple-init.cabal
├── src/
│   └── Lib.hs
├── stack.yaml
├── stack.yaml.lock
└── test/
    └── Spec.hs
```

- `app/Main.hs`: 엔트리 포인트. `Lib.someFunc`를 호출합니다.
- `src/Lib.hs`: PostgreSQL 접속/CRUD 로직과 초기 테이블 생성(`initDB`).
- `docker-compose.yaml`: 로컬 PostgreSQL 17.6 컨테이너 실행 설정.
- `Dockerfile`: Stack 기반 컨테이너 빌드 및 실행.
- `Makefile`: 자주 쓰는 작업을 단축 명령으로 제공.

## How to run

1. PostgreSQL 실행

```bash
docker compose up -d
# 포트: 5432, 사용자/비밀번호/DB: postgres/postgres/postgres
```

1. 의존성 설치

- Install sd (macOS: Homebrew)

```bash
brew install sd
```

- Install stylish-haskell (macOS: Homebrew)

```bash
brew install stylish-haskell
```

1. 빌드

```bash
# Makefile 사용
make setup
make build
```

또는 직접 stack 사용:

```bash
stack setup
stack build
```

1. 실행

```bash
# Makefile 사용
make run
```

또는 직접 stack 사용:

```bash
stack exec postgresql-simple-init-exe
```

실행 시 `src/Lib.hs`의 `someFunc`가 다음을 수행합니다.

- `initDB`: `test` 테이블이 없으면 생성
- INSERT → UPDATE → SELECT → DELETE 순으로 동작 예시 실행 및 결과 출력

## Configuration

- 기본 접속 정보(`src/Lib.hs`):

```haskell
localPG = defaultConnectInfo
  { connectHost = "127.0.0.1"
  , connectDatabase = "postgres"
  , connectUser = "postgres"
  , connectPassword = "postgres"
  }
```

- 로컬에서 `docker-compose.yaml`로 띄운 컨테이너 설정과 일치합니다.

## Make targets

- `update-stack-yaml`: `stack.yaml`과 `Dockerfile`의 프로젝트명 관련 값을 최신 디렉터리명으로 치환(sd 사용)
- `setup`: `stack setup`과 테스트 의존성 설치
- `build`: 빠른 빌드 옵션으로 빌드
- `test`: 테스트 실행
- `run`: 실행 파일 실행
- `docker-build`: 도커 이미지를 빌드합니다.
- `docker-run`: 이미지를 실행합니다.

참고: `update-stack-yaml`는 Rust 기반 치환 도구인 `sd`를 사용합니다. macOS(Homebrew): `brew install sd`

## References

- <https://tuttlem.github.io/2020/10/30/postgresql-data-access-with-haskell.html>
