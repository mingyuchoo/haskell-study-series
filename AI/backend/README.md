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

This project follows Clean Architecture principles:

```
src/
├── Lib.hs               # Low-level Azure OpenAI HTTP client
├── Domain/              # Business entities and interfaces
│   ├── Entities.hs     # Core domain models
│   └── Ports.hs        # Service interfaces
├── Application/         # Use cases
│   └── UseCases.hs     # Business logic
├── Infrastructure/      # External services
│   └── AzureOpenAI.hs  # Azure OpenAI adapter (wraps Lib.hs)
└── Presentation/        # API layer
    ├── API.hs          # API definition
    └── Server.hs       # Server setup
```

## Features

- REST API with Servant
- Clean Architecture design
- OpenAPI/Swagger documentation
- Web UI for chat
- Health check endpoint
- Type-safe API interactions
- Environment variable configuration
