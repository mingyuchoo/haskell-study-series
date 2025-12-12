#!/bin/bash

# Build script for Haskell LSP VSCode Extension

set -e

echo "Installing dependencies..."
npm install

echo "Compiling TypeScript..."
npm run compile

echo "Running linter..."
npm run lint

echo "Build completed successfully!"
echo "To package the extension, run: vsce package"