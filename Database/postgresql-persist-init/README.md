# postgresql-persist-init

A small Haskell example project demonstrating how to:

- Use Persistent with PostgreSQL for schema definition and CRUD access
- Use Esqueleto for type-safe SQL-like queries and joins
- Expose a simple REST API with Servant and Warp
- Integrate Redis as a simple cache layer
- Build and test with Stack
- Containerize with Docker

This project provides three server variants and database migration executables to showcase patterns with Persistent and Esqueleto.

## Features

- Basic `User` model (Persistent) and extended `Article` model (Esqueleto)
- REST endpoints to create and fetch users
- Redis-backed user caching example and tests
- Makefile targets for common dev flows
- Dockerfile and docker-compose for local services

## Project Structure

```shell
app/
  MigrateDB.hs        # Run DB migrations for Persistent schema
  MigrateDBEsq.hs     # Run DB migrations for Esqueleto schema
  RunServer.hs        # Entry point to run Basic/Cache/Esq servers
src/
  DB/Cache.hs       # Redis cache helpers for User
  DB/Basic.hs         # Persistent-based DB helpers (User)
  DB/Esq.hs           # Esqueleto-based DB helpers (User, Article)
  Samples/Objects.hs  # Sample in-memory objects for quick examples
  Schema/Basic.hs     # Persistent schema: User
  Schema/Esq.hs       # Persistent schema: User, Article + JSON instances
  Server/Basic.hs     # Servant server exposing user endpoints (port 8000)
  Server/Cache.hs     # Servant server variant with caching (referenced by tests)
  Server/Esq.hs       # Servant server variant using Esqueleto
 test/
  APITests.hs         # Hspec tests for cache server behavior
  TestUtils.hs        # Test harness: spins up server, migrates DB
```

Note: `Server.Cache` and `Server.Esq` are referenced by executables and tests and are expected to follow the same API surface as `Server.Basic`.

## Prerequisites

- Stack (resolver: `lts-24.4`)
- Docker (optional, for containers)
- Docker Compose (optional, to start Postgres and Redis)

## Dependencies

See `package.yaml` and the generated `postgresql-persist-init.cabal`. Key libraries:

- `persistent`, `persistent-postgresql`, `persistent-template`
- `esqueleto`
- `servant`, `servant-server`, `servant-client`
- `warp`
- `hedis` (Redis)
- `aeson`

## Services

Local services for development/testing are provided via `docker-compose.yaml`:

- Postgres 17.x on `127.0.0.1:5432` with `postgres/postgres`
- Redis 8.2 on `127.0.0.1:6379`

Start them with:

```shell
make docker-up
```

Stop them with:

```shell
make docker-down
```

The code expects the following default connection strings:

- PostgreSQL: `host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres` (see `DB/Basic.hs` and `DB/Esq.hs`)
- Redis: `defaultConnectInfo` (see `DB/Cache.hs`)

## Build, Run, Test

### Setup and build

```shell
make setup
make build
```

### Run servers

There are three variants; `app/RunServer.hs` dispatches based on arguments.

- Basic server (Persistent):

  ```shell
  make run
  ```

  Equivalent to:

  ```shell
  stack exec run-server
  ```

- Cache server (uses Redis for simple caching):

  ```shell
  make run-cache
  ```

  Equivalent to:

  ```shell
  stack exec run-server -- cache
  ```

- Esqueleto server:

  ```shell
  make run-esq
  ```

  Equivalent to:

  ```shell
  stack exec run-server -- esq
  ```

By default `Server.Basic.runServer` listens on port `8000`.

### Database migrations

- Persistent schema (Basic):

  ```shell
  make migrate
  ```

  Equivalent to `stack exec migrate-db`.

- Esqueleto schema:

  ```shell
  make migrate-esq
  ```

  Equivalent to `stack exec migrate-db -- esq` or `stack exec migrate-db-esq`.

### Run tests

Tests start the cache server and validate behavior across Postgres and Redis.

```shell
make test
```

Watch modes are available:

```shell
make watch-test
make watch-coverage
```

## API (Server.Basic)

`src/Server/Basic.hs` defines the API and handlers. Base URL: `http://127.0.0.1:8000`.

- `GET /` → List endpoints
- `POST /users` with JSON body `User` → returns `Int64` (new user ID)
- `GET /users` → returns `[Entity User]`
- `GET /users/{id}` → returns `User` or 401 if not found

### Example curl

- List endpoints

  ```shell
  curl -s http://127.0.0.1:8000/
  ```

- Create user

  ```shell
  curl -s -X POST http://127.0.0.1:8000/users \
    -H 'Content-Type: application/json' \
    -d '{
      "name": "james",
      "email": "james@test.com",
      "age": 25,
      "occupation": "Software Engineer"
    }'
  ```

- Fetch user by ID

  ```shell
  curl -s http://127.0.0.1:8000/users/1
  ```

- List users

  ```shell
  curl -s http://127.0.0.1:8000/users
  ```

## Docker

A multi-stage-like single-image build using `fpco/stack-build:lts-24.4` is in `Dockerfile`.

Build and run:

```shell
make docker-build
make docker-run
```

Notes:

- The Dockerfile builds the executable and copies it to `/app/bin/app-exe`.
- The container prints a message about port 8000 and exposes 8000, but `Server.Basic` runs Warp on port 8000. If you run the image directly, either:
  - Map host port 80 to container port 8000 as written and adjust the server to bind 8000, or
  - Change the container to expose 8000 and run on 8000.

Adjust as needed for your preferred port mapping.

## Sample Data

See `src/Samples/Objects.hs` for example `User` and `Article` constructors you can use when playing in `ghci` or writing seeds.

## Makefile Targets

- `make setup` — stack setup and test deps
- `make build` — fast build with parallel GC
- `make run` — run basic server
- `make run-cache` — run cache server
- `make run-esq` — run esqueleto server
- `make migrate` — DB migration (basic)
- `make migrate-esq` — DB migration (esqueleto)
- `make test` — run tests (ensures docker services up)
- `make docker-up` / `make docker-down` — start/stop Postgres and Redis
- `make docker-build` — build Docker image
- `make docker-run` — run Docker image (host 80 → container 8000)

## Development Tips

- Use `stack ghci` or `ghcid` for rapid iteration. A helper target is provided:

  ```shell
  make ghcid
  ```

- Format sources:

  ```shell
  make format
  ```

## License

BSD-3-Clause. See `LICENSE`.
