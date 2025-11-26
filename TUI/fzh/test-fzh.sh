#!/bin/bash

# fzh 테스트 스크립트

echo "=== Building fzh ==="
stack build

echo ""
echo "=== Test 1: Simple file list ==="
echo -e "file1.txt\nfile2.txt\ntest.hs\nmain.hs\nREADME.md" | stack exec fzh-exe

echo ""
echo "=== Test 2: Current directory files ==="
find . -type f -name "*.hs" | stack exec fzh-exe

echo ""
echo "=== Running benchmarks ==="
stack bench

echo ""
echo "=== Running tests ==="
stack test
