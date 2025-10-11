# azure-openai-backend

A Haskell REST API service for Azure OpenAI multi-turn conversations, built with Clean Architecture.

## Prerequisites

- GHC (Glasgow Haskell Compiler) >= 9.2
- Cabal >= 3.6 or Stack

## Setup

1. Copy `.env.example` to `.env` and fill in your Azure OpenAI credentials:

```bash
cp .env.example .env
```

2. Install dependencies:

```bash
stack build
```

## Running

```bash
stack run
```

The server will start on http://localhost:8000

## API Endpoints

- **Web UI**: http://localhost:8000/
- **Chat API**: POST http://localhost:8000/api/chat
- **Health Check**: GET http://localhost:8000/health
- **Swagger UI**: http://localhost:8000/swagger-ui
- **OpenAPI JSON**: http://localhost:8000/openapi.json

## API Usage

### Chat Request

```bash
curl -X POST http://localhost:8000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "chatMessages": [
      {"msgRole": "user", "msgContent": "Hello!"}
    ]
  }'
```

### Health Check

```bash
curl http://localhost:8000/health
```

## Architecture

This project follows Clean Architecture principles with clear separation of concerns:

```
src/
├── Lib.hs                      # Low-level Azure OpenAI HTTP client
│                               # (handles HTTP requests, JSON parsing, streaming)
├── Domain/                     # Core business layer (framework-independent)
│   ├── Entities.hs            # Domain models: ChatMessage, ChatRole, ChatSession
│   └── Ports.hs               # Interfaces: ChatService, ChatConfig
├── Application/                # Business logic layer
│   └── UseCases.hs            # Use cases: sendChatMessage, streamChatMessage
├── Infrastructure/             # External adapters
│   └── AzureOpenAI.hs         # ChatService implementation (wraps Lib.hs)
└── Presentation/               # API/UI layer
    ├── API.hs                 # Servant API definition, DTOs, Swagger docs
    └── Server.hs              # Server setup, config loading, Warp runner
```

### Layer Responsibilities

- **Lib.hs**: Direct Azure OpenAI API integration with HTTP client and streaming support
- **Domain**: Pure business entities and port interfaces (no external dependencies)
- **Application**: Thin use case layer delegating to ChatService interface
- **Infrastructure**: Concrete ChatService implementation using Azure OpenAI
- **Presentation**: REST API endpoints, request/response DTOs, and server configuration

## Features

- REST API with Servant framework
- Clean Architecture with dependency inversion
- OpenAPI 3.0/Swagger documentation
- Interactive web UI for chat
- Health check endpoint
- Type-safe API with compile-time guarantees
- Environment variable configuration with .env support
- Streaming and non-streaming chat completions
