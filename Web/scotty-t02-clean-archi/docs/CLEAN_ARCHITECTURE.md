# Clean Architecture Refactoring

This document explains how the URL shortener has been refactored to follow Clean Architecture principles.

## Architecture Overview

The application is now organized into four distinct layers:

### 1. Domain Layer (`src/Domain/`)
Contains the core business logic and entities, independent of external concerns.

- **`Domain.Entity.Url`**: Core URL entity with validation logic
- **`Domain.Repository.UrlRepository`**: Repository interface (dependency inversion)

### 2. Application Layer (`src/Application/`)
Contains use cases that orchestrate domain entities and repositories.

- **`Application.UseCase.ShortenUrl`**: Use case for creating short URLs
- **`Application.UseCase.RetrieveUrl`**: Use case for retrieving original URLs
- **`Application.UseCase.ListUrls`**: Use case for listing all URLs

### 3. Infrastructure Layer (`src/Infrastructure/`)
Contains implementations of external concerns like data storage.

- **`Infrastructure.Repository.InMemoryUrlRepository`**: In-memory implementation of URL repository

### 4. Adapters Layer (`src/Adapters/`)
Contains controllers and views that handle external interfaces.

- **`Adapters.Web.Controller.UrlController`**: HTTP request handlers
- **`Adapters.Web.View.UrlView`**: HTML rendering logic

## Key Benefits

1. **Separation of Concerns**: Each layer has a single responsibility
2. **Dependency Inversion**: Core business logic doesn't depend on external frameworks
3. **Testability**: Each layer can be tested independently
4. **Maintainability**: Changes in one layer don't affect others
5. **Flexibility**: Easy to swap implementations (e.g., database storage)

## Dependency Flow

```
Adapters Layer → Application Layer → Domain Layer
     ↓
Infrastructure Layer → Domain Layer
```

- Adapters layer depends on Application layer
- Application layer depends on Domain layer  
- Infrastructure layer implements Domain interfaces
- Domain layer has no external dependencies

## Running the Application

```bash
stack build
stack exec scotty-t02-clean-archi-exe
```

The application will start on port 8000 with the same functionality as before, but now with a clean, maintainable architecture.