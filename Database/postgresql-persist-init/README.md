# postgresql-persist-init

A small Haskell example project demonstrating how to:

- Use Persistent with PostgreSQL for schema definition and CRUD access
- Use Esqueleto for type-safe SQL-like queries and joins
- Expose a simple REST API with Servant and Warp
- Integrate Redis as a simple cache layer
- Build and test with Stack
- Containerize with Docker

This project provides three server variants and database migration executables to showcase patterns with Persistent and Esqueleto.

**Note**: This project has been refactored to follow Clean Architecture principles. See `CLEAN_ARCHITECTURE.md` for detailed documentation of the new structure.

**Test Status**: The existing tests in the `test/` directory still reference the old module structure and will need to be updated to work with the new Clean Architecture. The core functionality has been preserved but the module organization has changed.

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
  Domain/             # Pure business logic (innermost layer)
    Entities/         # Business entities with validation
    Repositories/     # Repository interfaces (ports)
    Services/         # Service interfaces for external dependencies
  UseCases/           # Application business rules
    User/             # User-related use cases
  Interface/          # Interface adapters
    Web/              # Web controllers and DTOs
  Application/        # Service orchestration and dependency injection
  Infrastructure/     # Framework implementations (outermost layer)
    Persistence/      # Database implementations
    Cache/            # Cache implementations
    Web/              # Server configuration

 test/
  APITests.hs         # Hspec tests for cache server behavior
  TestUtils.hs        # Test harness: spins up server, migrates DB
```

Note: The server variants are now implemented using Clean Architecture principles in `src/Infrastructure/Web/Server.hs`.

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

## API

The API is now defined in `src/Interface/Web/Controllers/UserController.hs` with Clean Architecture principles. Base URL: `http://127.0.0.1:8000`.

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

### Full Stack with Docker Compose

The project includes a complete Docker Compose setup that runs PostgreSQL, Redis, and the Haskell application together:

```shell
# Start all services (database, cache, and app)
make docker-up-all

# Or use docker-compose directly
docker compose -f docker/docker-compose.yaml up -d

# View logs
make docker-logs

# Stop all services
make docker-down
```

The application will be available at `http://localhost:8000`.

### Development with Docker Services

For development, you can run just the database services in Docker and run the Haskell app locally:

```shell
# Start only PostgreSQL and Redis
make docker-up

# Run the app locally
make run
```

### Building the Application Image

Build and run just the Haskell application:

```shell
make docker-build
make docker-run
```

### Docker Services

The `docker/docker-compose.yaml` defines:

- **PostgreSQL 17.6**: Available on port 5432 with persistent volume
- **Redis 8.2**: Available on port 6379 with persistent volume  
- **Haskell App**: Built from `docker/Dockerfile`, runs on port 8000

All services include health checks and the app waits for database services to be ready before starting.

## Sample Data

For testing and development, you can create sample `User` and `Article` objects using the domain entities in `src/Domain/Entities/`.

## Makefile Targets

### Development
- `make setup` — stack setup and test deps
- `make build` — fast build with parallel GC
- `make run` — run basic server
- `make run-cache` — run cache server
- `make run-esq` — run esqueleto server
- `make migrate` — DB migration (basic)
- `make migrate-esq` — DB migration (esqueleto)
- `make test` — run tests (ensures docker services up)

### Docker Services
- `make docker-up` — start Postgres and Redis only
- `make docker-up-all` — start all services including app
- `make docker-down` — stop all services
- `make docker-logs` — show logs for all services
- `make docker-build-app` — build just the app service
- `make docker-restart-app` — restart just the app service

### Docker Build
- `make docker-build` — build app with docker-compose
- `make docker-run` — run app with docker-compose

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
