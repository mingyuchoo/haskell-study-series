# Clean Architecture Refactoring

This document describes the Clean Architecture refactoring of the postgresql-persist-init project.

## Architecture Overview

The refactored codebase follows Clean Architecture principles with clear separation of concerns across four main layers:

### 1. Domain Layer (Innermost)
**Location**: `src/Domain/`

Contains pure business logic with no external dependencies:

- **Entities** (`src/Domain/Entities/`): Core business objects with validation
  - `User.hs`: User entity with smart constructors and validation
  - `Article.hs`: Article entity with smart constructors and validation

- **Repository Interfaces** (`src/Domain/Repositories/`): Abstract contracts for data access
  - `UserRepository.hs`: Interface for user data operations
  - `ArticleRepository.hs`: Interface for article data operations

- **Service Interfaces** (`src/Domain/Services/`): Abstract contracts for external services
  - `CacheService.hs`: Interface for caching operations

### 2. Use Cases Layer (Application Business Rules)
**Location**: `src/UseCases/`

Contains application-specific business rules:

- **User Use Cases** (`src/UseCases/User/`):
  - `CreateUser.hs`: Create new user with validation and duplicate checking
  - `GetUser.hs`: Retrieve user with optional caching
  - `UpdateUser.hs`: Update user with validation and cache invalidation
  - `DeleteUser.hs`: Delete user with cache cleanup
  - `ListUsers.hs`: List all users

Each use case defines:
- Input/Output DTOs
- Business logic implementation
- Error handling

### 3. Interface Adapters Layer
**Location**: `src/Interface/` and `src/Application/`

Converts data between use cases and external interfaces:

- **Web Controllers** (`src/Interface/Web/Controllers/`):
  - `UserController.hs`: HTTP request/response handling

- **DTOs** (`src/Interface/Web/DTOs/`):
  - `UserDTO.hs`: Data transfer objects for web API

- **Application Services** (`src/Application/`):
  - `UserService.hs`: Orchestrates use cases and dependency injection

### 4. Infrastructure Layer (Outermost)
**Location**: `src/Infrastructure/`

Contains framework and external system implementations:

- **Database** (`src/Infrastructure/Persistence/PostgreSQL/`):
  - `UserRepositoryImpl.hs`: PostgreSQL implementation of UserRepository

- **Cache** (`src/Infrastructure/Cache/Redis/`):
  - `CacheServiceImpl.hs`: Redis implementation of CacheService

- **Web Server** (`src/Infrastructure/Web/`):
  - `Server.hs`: Warp server configuration and dependency wiring

## Key Benefits

### 1. Dependency Inversion
- Inner layers define interfaces (ports)
- Outer layers implement these interfaces (adapters)
- Dependencies point inward, not outward

### 2. Testability
- Business logic is isolated and pure
- Easy to mock external dependencies
- Use cases can be tested independently

### 3. Flexibility
- Easy to swap implementations (PostgreSQL → MongoDB, Redis → Memcached)
- Multiple server variants (basic, cached, esqueleto) share the same core logic
- Framework-agnostic business logic

### 4. Maintainability
- Clear separation of concerns
- Single responsibility principle
- Open/closed principle for extensions

## Migration Guide

### From Old Structure to Clean Architecture

**Old Structure:**
```
src/
├── DB/           # Data access mixed with business logic
├── Schema/       # Database schemas
├── Server/       # Web handlers mixed with business logic

```

**New Structure:**
```
src/
├── Domain/           # Pure business logic
│   ├── Entities/     # Business entities
│   ├── Repositories/ # Data access interfaces
│   └── Services/     # External service interfaces
├── UseCases/         # Application business rules
├── Interface/        # Controllers and DTOs
├── Application/      # Service orchestration
└── Infrastructure/   # Framework implementations
```

### Key Changes

1. **Entities**: Moved from database schemas to domain entities with validation
2. **Repositories**: Extracted interfaces from concrete implementations
3. **Use Cases**: Separated business logic from web handlers
4. **Controllers**: Focused only on HTTP concerns
5. **Services**: Dependency injection and orchestration

## Running the Refactored Application

The application entry points remain the same:

```bash
# Basic server (no cache)
make run
# or
stack exec run-server

# Cached server (with Redis)
make run-cache  
# or
stack exec run-server -- cache

# Esqueleto server
make run-esq
# or
stack exec run-server -- esq
```

## Testing Strategy

The Clean Architecture enables comprehensive testing at each layer:

1. **Unit Tests**: Test domain entities and use cases in isolation
2. **Integration Tests**: Test repository implementations with real databases
3. **API Tests**: Test controllers with mocked use cases
4. **End-to-End Tests**: Test complete workflows

## Future Enhancements

The Clean Architecture makes it easy to add:

1. **New Use Cases**: Add business logic without touching infrastructure
2. **New Adapters**: Support different databases, caches, or web frameworks
3. **Cross-Cutting Concerns**: Logging, monitoring, security
4. **Event Sourcing**: Add event publishing to use cases
5. **CQRS**: Separate read and write models

## Dependencies

The refactored code maintains the same external dependencies but organizes them by layer:

- **Domain**: No external dependencies (pure Haskell)
- **Use Cases**: Only domain dependencies
- **Interface**: Servant, Aeson for web concerns
- **Infrastructure**: Persistent, Redis, Warp for specific implementations