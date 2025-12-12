#!/bin/bash

# Comprehensive build script for Haskell LSP Extension
# Builds both the Haskell LSP server and the VSCode extension

set -e

echo "=========================================="
echo "Building Haskell LSP Extension"
echo "=========================================="

# Build the Haskell LSP server
echo "Step 1: Building Haskell LSP Server..."
echo "------------------------------------------"
stack build --fast
echo "✓ Haskell LSP server built successfully"

# Copy the server executable to a known location
echo "Step 2: Copying server executable..."
echo "------------------------------------------"
SERVER_EXE=$(stack exec -- which HaskellLSP-exe)
mkdir -p dist/
cp "$SERVER_EXE" dist/haskell-lsp-server
echo "✓ Server executable copied to dist/haskell-lsp-server"

# Build the VSCode extension
echo "Step 3: Building VSCode Extension..."
echo "------------------------------------------"
cd vscode-extension

# Install dependencies
echo "Installing npm dependencies..."
npm install

# Compile TypeScript
echo "Compiling TypeScript..."
npm run compile

# Run linter
echo "Running linter..."
npm run lint

echo "✓ VSCode extension built successfully"

# Package the extension
echo "Step 4: Packaging VSCode Extension..."
echo "------------------------------------------"
npm run package
echo "✓ Extension packaged successfully"

# Move the packaged extension to the root dist directory
mv *.vsix ../dist/
cd ..

echo "=========================================="
echo "Build Complete!"
echo "=========================================="
echo "Artifacts created:"
echo "  - dist/haskell-lsp-server (LSP server executable)"
echo "  - dist/*.vsix (VSCode extension package)"
echo ""
echo "To install the extension:"
echo "  code --install-extension dist/*.vsix"
echo ""
echo "To run the server manually:"
echo "  ./dist/haskell-lsp-server"