# Frontend-Backend Integration Guide

## Changes Made

### 1. Makefile Updates

Added frontend build integration:
- `frontend-build`: Builds React app and copies to `backend/static/`
- `frontend-clean`: Cleans frontend build artifacts
- Updated `build` and `release` targets to include `frontend-build`
- Updated `clean` target to include `frontend-clean`

### 2. Frontend Configuration (vite.config.ts)

- Set `base: '/'` for root path serving
- Configured build output to `dist/`
- Added proxy configuration for `/api` and `/health` endpoints to backend (for dev mode)

### 3. Backend Configuration

**package.yaml:**
- Added `data-files: static/**/*` to include frontend build in package

**API.hs:**
- Configured with `Raw` endpoint for static file serving
- Serves files from `static/` directory using `serveDirectoryFileServer "static"`

**Server.hs:**
- Added log messages for Web UI URL and all available endpoints

**.gitignore:**
- Added `static/` to ignore frontend build output

## How It Works

### Build Flow

```
1. make build
   ↓
2. frontend-build
   ↓
3. cd frontend && pnpm install && pnpm run build
   ↓
4. mkdir -p backend/static
   ↓
5. cp -r frontend/dist/* backend/static/
   ↓
6. stack build (backend)
```

### Runtime Flow

```
User Request → Backend Server (port 8000)
                    ↓
        ┌───────────┴───────────┐
        ↓                       ↓
    /api/chat              / (root)
    /health                    ↓
        ↓                  Static Files
    API Handler           (from static/)
```

### Development Workflow

**Option 1: Full Stack (Production-like)**
```bash
make build  # Build both
make run    # Run backend serving frontend
# Access: http://localhost:8000/
```

**Option 2: Separate Dev Servers (Hot Reload)**
```bash
# Terminal 1: Backend
cd backend
stack build
stack exec backend-exe

# Terminal 2: Frontend
cd frontend
pnpm run dev
# Access: http://localhost:5173/ (proxies API to :8000)
```

## File Structure

```
project/
├── Makefile                    # Build orchestration
├── backend/
│   ├── package.yaml           # Added data-files
│   ├── .gitignore            # Added static/
│   ├── static/               # Frontend build (generated)
│   │   ├── index.html
│   │   ├── assets/
│   │   └── ...
│   └── src/
│       └── Presentation/
│           ├── API.hs             # Raw endpoint for static
│           └── Server.hs          # Updated logs
└── frontend/
    ├── vite.config.ts        # Updated with proxy
    ├── dist/                 # Build output (generated)
    └── src/
        └── App.tsx
```

## Testing the Integration

1. **Build everything:**
   ```bash
   make build
   ```

2. **Verify frontend build:**
   ```bash
   ls -la backend/static/
   # Should see: index.html, assets/, etc.
   ```

3. **Run the server:**
   ```bash
   make run
   ```

4. **Test endpoints:**
   ```bash
   # Web UI
   curl http://localhost:8000/
   
   # Health check
   curl http://localhost:8000/health
   
   # API
   curl -X POST http://localhost:8000/api/chat \
     -H "Content-Type: application/json" \
     -d '{"inputMessage":"Hello","sessionId":null}'
   ```

## Troubleshooting

### Frontend not showing
- Check if `backend/static/` exists and has files
- Run `make frontend-build` manually
- Check server logs for static file serving errors

### API calls failing from frontend
- Verify backend is running on port 8000
- Check CORS configuration (already enabled with `simpleCors`)
- Check browser console for errors

### Build fails
- Ensure pnpm is installed: `bun install -g pnpm`
- Ensure Stack is installed and configured
- Run `make clean` and try again

## Next Steps

To enhance the integration:

1. **Update Frontend App** to call the backend API
2. **Add environment variables** for API URL configuration
3. **Configure Docker** to include frontend build
4. **Add CI/CD** pipeline for automated builds
5. **Add frontend tests** to the build process
