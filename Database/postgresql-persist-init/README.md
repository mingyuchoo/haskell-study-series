# postgresql-persist-init

A small Haskell example project demonstrating how to:

- Use Persistent with PostgreSQL for schema definition and CRUD access
- Use Esqueleto for type-safe SQL-like queries and joins
- Expose a simple REST API with Servant and Warp
- Integrate Redis as a simple cache layer
- Build and test with Stack
- Containerize with Docker

This project provides three server variants and database migration executables to showcase patterns with Persistent and Esqueleto.

**Note**: This project has been refactored to follow Clean Architecture principles. See `docs/CLEAN_ARCHITECTURE.md` for detailed documentation of the new structure.

**Test Status**: The tests in the `test/` directory have been updated to work with the new Clean Architecture. The core functionality has been preserved and the module organization follows Clean Architecture patterns.

## Features

- Basic `User` model (Persistent) and extended `Article` model (Esqueleto)
- REST endpoints to create and fetch users
- Redis-backed user caching example and tests
- Makefile targets for common dev flows
- Dockerfile and docker-compose for local services

## Project Structure

This project follows **Clean Architecture** principles with clear separation of concerns:

```shell
app/
  MigrateDB.hs        # Run DB migrations for Persistent schema
  MigrateDBEsq.hs     # Run DB migrations for Esqueleto schema
  RunServer.hs        # Entry point to run Basic/Cache/Esq servers

src/
  Domain/             # Pure business logic (innermost layer)
    Entities/         # Business entities with smart constructors and validation
      User.hs         # User entity with validation rules
      Article.hs      # Article entity with validation rules
    Repositories/     # Repository interfaces (ports)
      UserRepository.hs     # Abstract user data access interface
      ArticleRepository.hs  # Abstract article data access interface
    Services/         # Service interfaces for external dependencies
      CacheService.hs # Abstract caching interface

  UseCases/           # Application business rules
    User/             # User-related use cases
      CreateUser.hs   # Create user with validation and duplicate checking
      GetUser.hs      # Retrieve user with optional caching
      UpdateUser.hs   # Update user with validation and cache invalidation
      DeleteUser.hs   # Delete user with cache cleanup
      ListUsers.hs    # List all users

  Interface/          # Interface adapters
    Web/              # Web layer adapters
      Controllers/    # HTTP request/response handling
        UserController.hs  # User API endpoints
      DTOs/           # Data transfer objects
        UserDTO.hs    # Web API data structures

  Application/        # Service orchestration and dependency injection
    UserService.hs    # Coordinates use cases and manages dependencies

  Infrastructure/     # Framework implementations (outermost layer)
    Persistence/      # Database implementations
      PostgreSQL/
        UserRepositoryImpl.hs  # PostgreSQL user repository
    Cache/            # Cache implementations
      Redis/
        CacheServiceImpl.hs    # Redis cache service
    Web/              # Server configuration
      Server.hs       # Warp server setup and dependency wiring

test/
  APITests.hs         # Integration tests for API endpoints
  TestUtils.hs        # Test utilities and setup
```

**Architecture Benefits:**
- **Dependency Inversion**: Inner layers define interfaces, outer layers implement them
- **Testability**: Business logic is isolated and easily testable
- **Flexibility**: Easy to swap implementations (PostgreSQL → MongoDB, Redis → Memcached)
- **Maintainability**: Clear separation of concerns and single responsibility

For detailed architecture documentation, see `docs/CLEAN_ARCHITECTURE.md`.

**Migration Status**: The project has been successfully refactored from the old mixed-concern structure to Clean Architecture. All old files (`src/DB/`, `src/Schema/`, `src/Server/`) have been replaced with their Clean Architecture equivalents while preserving functionality.

**Build Status**: The codebase compiles cleanly with minimal warnings. All unused imports have been removed and the code follows Clean Architecture dependency rules.

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

- PostgreSQL: `host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres` (see `src/Infrastructure/Persistence/PostgreSQL/UserRepositoryImpl.hs`)
- Redis: `defaultConnectInfo` (see `src/Infrastructure/Cache/Redis/CacheServiceImpl.hs`)

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

By default all server variants listen on port `8000`.

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

The API is defined in `src/Interface/Web/Controllers/UserController.hs` following Clean Architecture principles. Base URL: `http://127.0.0.1:8000`.

**Endpoints:**
- `GET /` → List available endpoints
- `POST /users` → Create new user (returns user ID)
- `GET /users` → List all users
- `GET /users/{id}` → Get user by ID
- `PUT /users/{id}` → Update user (full update)
- `PATCH /users/{id}` → Update user (partial update)
- `DELETE /users/{id}` → Delete user

**Data Flow:**
1. HTTP requests → `UserController` (Interface layer)
2. Controller → `UserService` (Application layer)
3. Service → Use Cases (Use Cases layer)
4. Use Cases → Repository/Cache interfaces (Domain layer)
5. Interfaces → Concrete implementations (Infrastructure layer)

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

For testing and development, you can create sample `User` and `Article` objects using the domain entities in `src/Domain/Entities/`. The entities include smart constructors with validation to ensure data integrity.

## Makefile Targets

### Development
- `make setup` — stack setup and test deps
- `make build` — fast build with parallel GC
- `make run` — run basic server
- `make run-cache` — run cache server
- `make run-esq` — run esqueleto server
- `make migrate` — DB migration (basic)
- `make migrate-esq` — DB migration (esqueleto)
- `make test` — run tests (automatically starts docker services and waits for readiness)
- `make watch-test` — run tests in watch mode
- `make watch-coverage` — run tests with coverage in watch mode
- `make coverage` — run tests with coverage report

### Docker Services
- `make docker-up` — start Postgres and Redis only
- `make docker-up-all` — start all services including app
- `make docker-down` — stop all services and remove volumes
- `make docker-logs` — show logs for all services
- `make docker-build-app` — build just the app service
- `make docker-restart-app` — restart just the app service
- `make wait-for` — wait for services to be ready (used internally by tests)

### Docker Build
- `make docker-build` — build Docker image for the app
- `make docker-run` — run Docker image (exposes port 8000)
- `make docker-build-multi` — build multi-stage Docker image
- `make docker-run-multi` — run multi-stage Docker image

## Development Tips

- Use `stack ghci` or `ghcid` for rapid iteration. A helper target is provided:

  ```shell
  make ghcid
  ```

- Format sources with stylish-haskell:

  ```shell
  make format
  ```

  Or use the format script directly:

  ```shell
  ./format.sh
  ```

- The project follows Clean Architecture principles with clear separation between:
  - **Domain**: Pure business logic (`src/Domain/`)
  - **Use Cases**: Application business rules (`src/UseCases/`)
  - **Interface**: Controllers and DTOs (`src/Interface/`)
  - **Infrastructure**: Framework implementations (`src/Infrastructure/`)

- Tests are organized to work with the Clean Architecture and can be run with:

  ```shell
  make test
  ```

## License

BSD-3-Clause. See `LICENSE`.
