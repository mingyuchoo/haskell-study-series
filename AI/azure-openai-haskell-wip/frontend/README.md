# OpenAI Chat Frontend

React + TypeScript frontend for the OpenAI Haskell Agent chat application.

## Features

- 💬 Real-time chat interface with OpenAI
- 🔄 Session management for conversation continuity
- 🎨 Clean, modern UI with gradient design
- ⚡ Built with React 19, TypeScript, and Vite
- 📱 Responsive design

## Prerequisites

- Node.js 18+ or compatible runtime
- pnpm (or npm/yarn)
- Backend server running (see `../backend/README.md`)

## Setup

1. Install dependencies:
```bash
pnpm install
```

2. Configure environment variables:
```bash
cp .env.example .env
```

Edit `.env` to set your backend API URL (default: `http://localhost:8000`)

## Development

Start the development server:
```bash
pnpm dev
```

The app will be available at `http://localhost:5173`

## Build

Build for production:
```bash
pnpm build
```

Preview production build:
```bash
pnpm preview
```

## API Integration

The frontend connects to the backend API with the following endpoints:

- `POST /api/chat` - Send chat messages
  - Request: `{ inputMessage: string, sessionId?: string }`
  - Response: `{ outputMessage: string, outputSessionId: string }`

- `GET /health` - Health check endpoint

## Environment Variables

- `VITE_API_URL` - Backend API base URL (default: `http://localhost:8000`)

## Tech Stack

- React 19
- TypeScript
- Vite
- CSS3 (no external UI libraries)
