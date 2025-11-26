#!/bin/bash

# fzh 데모 스크립트

echo "=== fzh Demo ==="
echo ""

echo "1. 간단한 파일 목록 검색"
echo "   (타이핑하여 필터링, Enter로 선택, Esc로 종료)"
echo ""
echo "   실행: find . -name '*.hs' | stack exec fzh-exe"
echo ""

echo "2. Git 파일 검색"
echo "   실행: git ls-files | stack exec fzh-exe"
echo ""

echo "3. 현재 디렉토리의 모든 파일"
echo "   실행: find . -type f | stack exec fzh-exe"
echo ""

echo "4. 프로세스 검색"
echo "   실행: ps aux | stack exec fzh-exe"
echo ""

echo "예제 실행 (Haskell 파일 검색):"
find . -name "*.hs" -type f | stack exec fzh-exe
