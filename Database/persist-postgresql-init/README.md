# persist-postgresql-init

## 요구 사항

### PostgreSQL

이 코드를 빌드하려면 시스템에 PostgreSQL이 설치되어 있어야 합니다. `stack build` 실행 시 아래와 같은 오류가 보인다면 PostgreSQL이 설치되어 있지 않다는 의미입니다:

```bash
>> stack build
setup: The program 'pg_config' is required but it could not be found
```

Linux에서는 적어도 다음 패키지가 필요합니다:

```bash
>> sudo apt-get update
>> sudo apt-get install postgresql-server-dev-all libpq-dev
```

Windows 및 macOS에서는 [여기](https://postgresql.org/download)의 다운로드를 사용하면 됩니다.

#### macOS (Homebrew) 설정

macOS, 특히 Apple Silicon(M1/M2/M3)에서는 Homebrew에서 `libpq`가 keg-only입니다. Stack 빌드에서 `pg_config`와 `libpq`를 인식하도록 다음 단계를 따르세요:

1) 필수 패키지 설치

```bash
brew update
brew install libpq pkg-config
# 선택: 로컬 PostgreSQL 서버 설치
brew install postgresql
```

2) 셸 환경(zsh) 설정 업데이트

PATH에 `libpq`의 bin을 추가하고 pkg-config 파일을 노출합니다. Apple Silicon의 Homebrew 기본 경로는 보통 `/opt/homebrew`입니다.

```bash
echo 'export PATH="/opt/homebrew/opt/libpq/bin:$PATH"' >> ~/.zshrc
echo 'export PKG_CONFIG_PATH="/opt/homebrew/opt/libpq/lib/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.zshrc
source ~/.zshrc
```

Intel macOS에서 Homebrew가 `/usr/local`에 설치되어 있다면 경로를 다음과 같이 조정하세요:

```bash
echo 'export PATH="/usr/local/opt/libpq/bin:$PATH"' >> ~/.zshrc
echo 'export PKG_CONFIG_PATH="/usr/local/opt/libpq/lib/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.zshrc
source ~/.zshrc
```

3) 정상 노출 여부 확인

```bash
which pg_config
pg_config --version
pkg-config --modversion libpq
```

4) Stack 힌트(감지가 계속 실패하는 경우)

`postgresql-libpq`가 헤더와 라이브러리를 찾을 수 있도록 `stack.yaml`에 include/lib 디렉터리를 지정할 수 있습니다:

```yaml
# stack.yaml
extra-include-dirs:
  - /opt/homebrew/opt/libpq/include   # 또는 /usr/local/opt/libpq/include
extra-lib-dirs:
  - /opt/homebrew/opt/libpq/lib       # 또는 /usr/local/opt/libpq/lib
```

5) GHC 버전을 resolver와 맞추기(GHC 불일치 오류가 보일 경우)

전역 GHC가 resolver의 GHC와 다르다면, 이 프로젝트는 Stack이 컴파일러를 관리하도록 하세요:

```bash
stack setup
stack build --no-system-ghc
```

이렇게 하면 시스템에 설치된 GHC 대신 Stackage resolver의 GHC가 사용됩니다.

### Redis

[파트 3](https://www.mmhaskell.com/real-world/redis)부터 서버에 [Redis](https://www.redis.io)를 이용한 캐싱을 도입합니다. 사용하는 운영체제에 맞춰 [설치 가이드](https://www.redis.io/topics/quickstart)를 따라 설치하세요.

중요하게 사용할 것은 서버 데몬과 클라이언트 CLI입니다. 서버는 `redis-server` 명령으로 백그라운드에 실행해두고, CLI는 `redis-cli`로 실행하여 기본 명령을 테스트할 수 있습니다:

```bash
>> redis-cli
> EXISTS "1"
1
> EXISTS "45"
0
```

### Docker

[파트 4](https://www.mmhaskell.com/real-world/docker)에서는 개발자 환경에 영향을 받지 않도록 Docker를 사용하여 테스트를 작성합니다. [Docker 홈페이지](https://docker.com)의 안내를 따라 시스템에 설치하고 실행하세요.

## 코드 실행

### 빌드

라이브러리와 실행 파일을 빌드합니다:

```bash
stack build
# 빠른 빌드 옵션
stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"
```

### 빠른 실행(실행 파일)

이 프로젝트는 다음 두 개의 실행 파일을 제공합니다:

- `migrate-db` — 데이터베이스 마이그레이션 실행
- `run-server` — 웹 서버 실행

마이그레이션 실행:

```bash
# Persistent 기반 마이그레이션
stack exec migrate-db

# Esqueleto 기반 마이그레이션
stack exec migrate-db -- esq
```

서버 실행:

```bash
# 기본 서버
stack exec run-server

# 캐시 서버(실행 중인 redis-server 필요)
stack exec run-server -- cache

# Esqueleto 서버
stack exec run-server -- esq
```

### [파트 1: Persistent](https://www.mmhaskell.com/real-world/databases)

이 파트의 코드는 GHCI에서 손쉽게 실행할 수 있습니다. 핵심은 Postgres 서버가 실행 중이어야 한다는 점입니다. 사용 환경에 맞도록 코드 내 [`localConnString` 변수](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/Database.hs#L17)를 수정하세요. 기본값은 사용자명, DB 이름, 비밀번호가 모두 "postgres"입니다:

```haskell
localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"
```

GHCI에서 코드를 로드한 후 `migrateDB` 식을 실행합니다. 그러면 스키마에 정의된 `users` 테이블로 Postgres 데이터베이스가 마이그레이션됩니다. 필요하다면 이곳에서도 연결 문자열을 바꿔서 사용할 수 있습니다.

```bash
>> stack ghci
>> :l
-- (모든 모듈을 제거하여 이름 충돌 방지)
>> import Database
>> let localConnString' = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres" :: PGInfo
>> migrateDB localConnString'
```

그다음 `Database` 모듈의 다른 함수들을 사용하여 쿼리를 실행해볼 수 있습니다:

```bash
>> let u = User "Kristina" "kristina@gmail.com" 45 "Software Engineer"
>> createUserPG localConnString u
1
>> fetchUserPG localConnString 1
Just (User {userName = "Kristina", userEmail = "kristina@gmail.com", userAge = 45, userOccupation = "Software Engineer"})
>> deleteUserPG localConnString 1
```

각 단계 이후에는 Postgres 데이터베이스에서도 쿼리가 성공했는지 확인할 수 있습니다. `psql` 명령으로 Postgres 터미널을 띄우고, `-U` 인자로 사용자명을 넘긴 뒤 비밀번호를 입력하세요. `\c`로 다른 데이터베이스에 접속할 수도 있습니다.

```
>> psql -U postgres
(비밀번호 입력)
>> \c postgres
>> select * from users;
(방금 생성한 사용자들을 확인!)
```

### [파트 2: Servant](https://www.mmhaskell.com/real-world/servant)

두 번째 파트에서는 데이터베이스의 정보를 노출하는 아주 기초적인 서버를 만듭니다. 소스는 [이 모듈](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/BasicServer.hs)을 참고하세요.

이 서버를 실행하기 전에, 1부에서 마이그레이션을 하지 않았다면 먼저 마이그레이션을 수행하세요:

```bash
>> stack exec migrate-db
```

그다음 아래 실행 파일로 서버를 띄웁니다:

```bash
>> stack exec run-server
```

이제 어떤 클라이언트 프로그램으로든 서버에 HTTP 요청을 보낼 수 있습니다. 필자는 [Postman](https://postman.com)을 선호합니다. 1부에서 했던 것과 같은 패턴으로 진행해보세요. 예를 들어 사용자 생성:

```bash
POST /users
{
  "name": "Kristina",
  "email": "kristina@gmail.com",
  "age": 45,
  "occupation": "Software Engineer"
}

...

2
```

이후 조회:

```bash
GET /users/2

...

{
  "name": "Kristina",
  "email": "kristina@gmail.com",
  "age": 45,
  "occupation": "Software Engineer"
}
```

존재하지 않는 사용자를 조회해보는 것도 가능합니다:

```bash
GET /users/45

...

Could not find user with that ID
```

### [파트 3: Redis](https://www.mmhaskell.com/real-world/redis)

시리즈의 세 번째 파트에서는 [Haskell에서 Redis 사용](https://hackage.haskell.org/package/hedis)을 다룹니다. 글의 코드 예시는 [Cache 모듈](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/Cache.hs)과
[CacheServer 모듈](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/CacheServer.hs)에서 확인할 수 있으며, 파트 2의 서버에 캐싱 기능을 추가한 버전입니다.

업데이트된 서버는 `run-server` 실행 파일에 `cache` 인자를 주어 실행합니다. 먼저 Redis 서버가 백그라운드에서 실행 중이어야 합니다:

```bash
>> redis-server &
>> stack exec run-server -- cache
```

외부 API는 동일하므로, 파트 2의 예시처럼 `GET /users/2`를 호출한 뒤 `redis-cli`에서 캐시에 저장된 결과를 확인할 수 있습니다:

```bash
>> redis-cli
> GET "2"
"User {userName = \"Kristina\", userEmail = \"kristina@gmail.com\", userAge = 45, userOccupation = \"Software Engineer\"}"
> GET "45"
(nil)
```

### [파트 4: Docker 테스트](https://www.mmhaskell.com/real-world/docker)

이 파트에서는 서버에 대한 테스트를 작성합니다. 일부 설정 코드는 [이 모듈](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/test/TestUtils.hs)에서 볼 수 있고, 핵심 검증은 Hspec으로 작성된 [이 파일](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/test/APITests.hs)에서 확인할 수 있습니다.

테스트 실행은 다음과 같습니다:

```bash
stack test
```

이 테스트는 로컬에서 Postgres 데이터베이스와 Redis 캐시가 실행 중이어야 합니다. 두 가지 방법이 있습니다. 첫째, 지금까지와 같이 로컬 시스템의 서비스를 사용합니다. 이 경우 테스트는 "부작용"이 있을 수 있습니다. 테스트가 정상 종료되면 테이블에 추가 행이 남지는 않지만, 사용자 삽입 후 삭제 과정 때문에 프라이머리 키 인덱스는 증가합니다.

둘째, Docker를 사용하는 방법입니다. 저장소의 루트 디렉터리에서 다음 명령으로 제공된 Docker 컨테이너를 실행합니다:

```bash
>> docker-compose up
```

자세한 구성은 [Docker Compose 파일](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/docker-compose.yml)에서 확인할 수 있습니다.

그다음 [stack.yaml](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/stack.yaml)에서 프로젝트 설정을 Docker 사용으로 변경합니다. `docker.enable` 플래그를 `true`로 바꾸세요:

```yaml
docker:
  enabled: true
```

이후 다시 `stack test`를 실행합니다. 최초 실행 시에는 컨테이너가 Stack, GHC 및 필요한 유틸리티를 다운로드하느라 시간이 더 걸리지만, 다음부터는 더 빨라집니다.

### [파트 5: Esqueleto](https://www.mmhaskell.com/real-world/esqueleto)

파트 5에서는 외래 키 관계를 포함하는 새 타입을 스키마에 추가합니다. 앞선 4개 파트의 코드와 충돌을 피하기 위해 별도의 모듈 집합을 사용합니다:

1. [새 스키마 모듈](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/SchemaEsq.hs)
2. [새 데이터베이스 라이브러리](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/DatabaseEsq.hs)
3. [업데이트된 서버](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/ServerEsq.hs) (이 서버에는 Redis 캐싱이 없습니다)
4. [샘플 객체](https://github.com/MondayMorningHaskell/RealWorldHaskell/blob/master/src/SampleObjects.hs) (데이터베이스 삽입용)

이 코드를 시험하려면 데이터베이스에 새 마이그레이션을 먼저 실행하세요:

```bash
>> stack exec migrate-db -- esq
```

이 작업은 `articles` 테이블을 추가하지만, `users` 테이블에는 영향을 주지 않으므로 기존 코드에는 문제가 없습니다.

그다음 두 가지 방법으로 데이터베이스를 갱신할 수 있습니다. 첫째, 1부와 마찬가지로 GHCI를 열어 직접 삽입을 실행합니다. `SampleObjects` 모듈에는 샘플로 사용할 객체들이 준비되어 있습니다. `User` 객체는 그대로 사용하면 되지만, `Article` 객체는 사용자 생성 후 그 사용자의 정수형 ID를 인자로 넘겨야 합니다. 예를 들어:

```bash
>> stack ghci
>> :l
>> :load DatabaseEsq SampleObjects
>> import SampleObjects
>> createUserPG localConnString testUser1
5
>> createArticlePG localConnString (testArticle1 5)
1
>> fetchRecentArticles
[(Entity {entityKey = SqlBackendKey 5, entityVal = User {...}}, Entity {entityKey = SqlBackendKey 1, entityVal = Article {...}})]
```

둘째, 서버를 통해 API를 사용하는 방법입니다. 먼저 업데이트된 서버를 실행합니다:

```bash
>> stack exec run-server -- esq
```

이후 Postman 등 선호하는 도구로 요청을 보냅니다:

```bash
POST /users
{
  "name": "Kristina",
  "email": "kristina@gmail.com",
  "age": 45,
  "occupation": "Software Engineer"
}

5

POST /articles

{
  "title": "First Post",
  "body": "A great description of our first blog post body.",
  "publishedTime": 1498914000,
  "authorId": 5
}

1

GET /articles/recent
[
  [
    {
      "name": "Kristina",
      "email": "kristina@gmail.com",
      "age": 45,
      "occupation": "Software Engineer",
      "id": 5
    },
    {
      "title": "First Post",
      "body": "A great description of our first blog post body.",
      "publishedTime": 1498914000,
      "authorId": 5,
      "id": 1
    }
  ]
]
```
