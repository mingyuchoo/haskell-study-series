# Frontend Integration Guide

This document describes how the frontend integrates with the Haskell backend API.

## Architecture Overview

```
┌─────────────────┐         ┌──────────────────┐
│  React Frontend │ ◄─────► │ Haskell Backend  │
│  (TypeScript)   │  HTTP   │   (Servant API)  │
└─────────────────┘         └──────────────────┘
```

## API Integration

### Chat Endpoint

**Frontend Implementation:**
```typescript
// src/api.ts
async sendMessage(input: ChatInput): Promise<ChatOutput> {
  const response = await fetch(`${API_BASE_URL}/api/chat`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(input),
  })
  return response.json()
}
```

**Backend Handler:**
```haskell
-- Presentation/Api/ApiHandler.hs
chatHandler :: ChatApplicationService s => s -> ChatInput -> Handler ChatOutput
chatHandler chatAppService chatInput = do
    (response, sessionUUID) <- liftIO $ handleChatRequest
        chatAppService
        (inputMessage chatInput)
        (sessionId chatInput)
    return $ ChatOutput
        { outputMessage = response
        , outputSessionId = sessionUUID
        }
```

### Type Mapping

| Frontend (TypeScript) | Backend (Haskell) | Description |
|----------------------|-------------------|-------------|
| `ChatInput` | `ChatInput` | Request payload |
| `ChatOutput` | `ChatOutput` | Response payload |
| `Message` | `Message` | Chat message entity |
| `HealthInfo` | `HealthInfo` | Health check response |

### Session Management

The frontend maintains session continuity by:
1. Receiving `outputSessionId` from the first chat response
2. Including `sessionId` in subsequent requests
3. Clearing session when user clicks "New Chat"

```typescript
// First message - no sessionId
{ inputMessage: "Hello" }

// Subsequent messages - include sessionId
{ inputMessage: "How are you?", sessionId: "uuid-from-previous-response" }
```

## Development Workflow

### Option 1: Integrated (Production-like)

1. Build frontend: `cd frontend && pnpm build`
2. Copy to backend: `cp -r dist/* ../backend/static/`
3. Run backend: `cd ../backend && stack run`
4. Access at: `http://localhost:8000/`

### Option 2: Separate (Development)

1. Run backend: `cd backend && stack run`
2. Run frontend: `cd frontend && pnpm dev`
3. Frontend at: `http://localhost:5173/`
4. Backend at: `http://localhost:8000/`

The frontend dev server proxies API requests to the backend automatically.

## Environment Configuration

### Frontend (.env)
```bash
VITE_API_URL=http://localhost:8000
```

### Backend (.env)
```bash
PORT=8000
OPENAI_API_KEY=your_key
OPENAI_API_URL=your_url
OPENAI_API_MODEL=gpt-4o
```

## CORS Configuration

The backend enables CORS using `simpleCors` middleware:

```haskell
-- Presentation/Server/Server.hs
app :: ChatApplicationService s => s -> Application
app chatAppService = simpleCors $ serve (Proxy :: Proxy API) (apiServer chatAppService)
```

This allows the frontend dev server to make cross-origin requests during development.

## Error Handling

### Frontend
```typescript
try {
  const data = await chatApi.sendMessage(input)
  // Handle success
} catch (err) {
  // Display error to user
  setError(err.message)
}
```

### Backend
- Returns appropriate HTTP status codes
- Servant automatically handles JSON parsing errors
- Application errors are caught and returned as 500

## Testing Integration

### Manual Testing
1. Start backend: `cd backend && stack run`
2. Start frontend: `cd frontend && pnpm dev`
3. Open browser: `http://localhost:5173/`
4. Send a test message
5. Verify response appears

### Health Check
```bash
curl http://localhost:8000/health
```

Expected response:
```json
{
  "status": "UP",
  "version": "0.1.0.0",
  "uptime": 0,
  "timestamp": "2025-10-10T12:00:00Z"
}
```

## Deployment

For production deployment:

1. Build frontend: `make frontend-build`
2. Build backend: `cd backend && stack build`
3. Run backend: `stack exec backend-exe`
4. Backend serves both API and static frontend files

The backend's `Raw` endpoint serves the frontend from the `static/` directory.
