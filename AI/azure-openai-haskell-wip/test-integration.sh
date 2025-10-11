#!/bin/bash

echo "=== Frontend-Backend Integration Test ==="
echo ""

# Check if frontend build exists
echo "1. Checking frontend build..."
if [ -d "backend/static" ] && [ -f "backend/static/index.html" ]; then
    echo "   ✓ Frontend build found in backend/static/"
    ls -lh backend/static/
else
    echo "   ✗ Frontend build not found!"
    echo "   Run: make frontend-build"
    exit 1
fi

echo ""

# Check if backend is built
echo "2. Checking backend build..."
if [ -f "backend/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743-tinfo6/b08f30752f772938ad7105c3de6ee196c83f93bcf0427cf54fcd154f36b15a7e/9.10.2/bin/backend-exe" ] || stack exec --cwd backend which backend-exe &>/dev/null; then
    echo "   ✓ Backend executable found"
else
    echo "   ✗ Backend not built!"
    echo "   Run: make build"
    exit 1
fi

echo ""

# Check configuration files
echo "3. Checking configuration..."
if [ -f "backend/.env" ]; then
    echo "   ✓ Backend .env file exists"
else
    echo "   ⚠ Backend .env file not found (copy from .env.example)"
fi

if [ -f "frontend/.env" ]; then
    echo "   ✓ Frontend .env file exists"
else
    echo "   ⚠ Frontend .env file not found (optional for production)"
fi

echo ""
echo "=== Integration Status: READY ==="
echo ""
echo "To run the server:"
echo "  make run"
echo ""
echo "Available endpoints:"
echo "  - Web UI:      http://localhost:8000/"
echo "  - API:         http://localhost:8000/api/chat"
echo "  - Health:      http://localhost:8000/health"
echo "  - Swagger UI:  http://localhost:8000/swagger-ui"
echo "  - OpenAPI:     http://localhost:8000/openapi.json"
