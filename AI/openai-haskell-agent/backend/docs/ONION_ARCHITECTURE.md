# Onion Architecture Refactoring

This project has been refactored to follow the Onion Architecture pattern, which consists of concentric layers with dependencies pointing inward.

## Architecture Layers

### 1. Domain Layer (Core)

The innermost layer containing business entities and interfaces.

- `Domain.Entities.*`: Core business entities
  - `Message.hs`: Defines the Message and Role types
  - `Chat.hs`: Defines ChatRequest, ChatResponse, Choice, and Delta types
  
- `Domain.Interfaces.*`: Core interfaces
  - `ChatService.hs`: Interface for chat services
  - `HttpClient.hs`: Interface for HTTP clients

### 2. Application Layer

Contains application services and use cases.

- `Application.Interfaces.*`: Application interfaces
  - `ChatApplicationService.hs`: Interface for chat application services
  
- `Application.Services.*`: Application services
  - `ChatService.hs`: Implementation of the ChatService interface
  - `ChatApplicationService.hs`: Implementation of the ChatApplicationService interface

### 3. Infrastructure Layer

Contains implementations of interfaces defined in inner layers.

- `Infrastructure.Http.*`: HTTP client implementations
  - `HttpClient.hs`: Implementation of the HttpClient interface
  
- `Infrastructure.OpenAI.*`: OpenAI-specific implementations
  - `OpenAIService.hs`: OpenAI service implementation

### 4. Presentation Layer

Contains UI/API controllers and adapters.

- `Presentation.Api.*`: API controllers
  - `ApiHandler.hs`: API endpoint handlers
  
- `Presentation.Server.*`: Server implementation
  - `Server.hs`: Web server implementation

## Benefits of Onion Architecture

1. **Separation of Concerns**: Each layer has a specific responsibility
2. **Dependency Rule**: Dependencies point inward
3. **Testability**: Core business logic can be tested without external dependencies
4. **Flexibility**: External frameworks and services can be replaced without affecting the core business logic
5. **Maintainability**: Changes in one layer don't affect other layers as long as interfaces remain stable
