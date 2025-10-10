# OpenAI Haskell Agent

Full-stack application with Haskell backend and React frontend.

## Architecture

- **Backend**: Haskell with Servant (Onion Architecture)
- **Frontend**: React + TypeScript + Vite
- **Integration**: Frontend builds are served as static files by the backend

## Quick Start

### Prerequisites

- Stack (Haskell build tool)
- Node.js and pnpm
- Make

### Build and Run

```bash
# Build everything (frontend + backend)
make build

# Run the server
make run
```

The server will start on port 8000:
- Web UI: http://localhost:8000/
- API: http://localhost:8000/api/chat
- Health: http://localhost:8000/health

### Development

#### Frontend Development (with hot reload)

```bash
cd frontend
pnpm install
pnpm run dev
```

This starts Vite dev server on port 5173 with proxy to backend API.

#### Backend Development

```bash
cd backend
make watch-test
```

### Build Process

The Makefile automatically:
1. Installs frontend dependencies
2. Builds the frontend (React app)
3. Copies frontend build to `backend/static/`
4. Builds the backend (Haskell)
5. Backend serves frontend from `/` and API from `/api`

### Project Structure

```
.
├── backend/          # Haskell backend
│   ├── src/         # Source code (Onion Architecture)
│   ├── test/        # Tests
│   └── static/      # Frontend build output (generated)
├── frontend/         # React frontend
│   ├── src/         # Source code
│   └── dist/        # Build output (generated)
└── Makefile         # Build orchestration
```

## Available Make Commands

- `make build` - Build frontend and backend
- `make run` - Run the server
- `make test` - Run backend tests
- `make clean` - Clean all build artifacts
- `make frontend-build` - Build only frontend
- `make frontend-clean` - Clean frontend build
- `make docker-build` - Build Docker image
- `make docker-run` - Run in Docker

## API Endpoints

### POST /api/chat
Chat with the AI assistant.

**Request:**
```json
{
  "inputMessage": "Hello",
  "sessionId": "optional-uuid"
}
```

**Response:**
```json
{
  "outputMessage": "Response from AI",
  "outputSessionId": "uuid"
}
```

### GET /health
Health check endpoint.

**Response:**
```json
{
  "status": "UP",
  "version": "0.1.0.0",
  "uptime": 0,
  "timestamp": "2025-01-10T12:00:00Z"
}
```
