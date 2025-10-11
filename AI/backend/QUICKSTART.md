# Quick Start Guide

## 1. Build the project

```bash
stack build
```

This will download and install all required dependencies including:
- servant (web framework)
- warp (web server)
- swagger2 (API documentation)

## 2. Configure environment

Make sure your `.env` file has:

```
AZURE_OPENAI_API_KEY=your-api-key
AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com
AZURE_OPENAI_DEPLOYMENT=your-deployment-name
AZURE_OPENAI_API_VERSION=2024-02-15-preview
```

## 3. Run the server

```bash
stack run
```

You should see:

```
Starting server on http://localhost:8000
Available endpoints:
  - Web UI: http://localhost:8000/
  - API: http://localhost:8000/api/chat
  - Health: http://localhost:8000/health
  - Swagger UI: http://localhost:8000/swagger-ui
  - OpenAPI JSON: http://localhost:8000/openapi.json
```

## 4. Test the API

### Using the Web UI

Open http://localhost:8000/ in your browser and start chatting!

### Using curl

```bash
curl -X POST http://localhost:8000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "chatMessages": [
      {"msgRole": "user", "msgContent": "What is Haskell?"}
    ]
  }'
```

### Check health

```bash
curl http://localhost:8000/health
```

### View API documentation

Open http://localhost:8000/swagger-ui in your browser.

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                  Presentation Layer                  │
│  (API.hs, Server.hs - REST endpoints, Swagger)      │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│                 Application Layer                    │
│        (UseCases.hs - Business logic)                │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│                   Domain Layer                       │
│  (Entities.hs, Ports.hs - Core models & interfaces) │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│               Infrastructure Layer                   │
│   (AzureOpenAI.hs - External service adapter)       │
└─────────────────────────────────────────────────────┘
```

## Clean Architecture Benefits

1. **Independence**: Business logic doesn't depend on frameworks
2. **Testability**: Easy to test each layer independently
3. **Flexibility**: Can swap Azure OpenAI for another provider
4. **Maintainability**: Clear separation of concerns
