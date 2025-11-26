# fzh 사용 가이드

## 설치

```bash
# 프로젝트 빌드
stack build

# 시스템에 설치
make install
```

설치 후 `~/.local/bin/fzh-exe`에 실행 파일이 생성됩니다.

## 기본 사용법

### 1. 파일 검색

```bash
# 현재 디렉토리의 모든 파일
find . -type f | fzh-exe

# Haskell 파일만
find . -name "*.hs" | fzh-exe

# Git 추적 파일
git ls-files | fzh-exe
```

### 2. 프로세스 검색

```bash
ps aux | fzh-exe
```

### 3. 히스토리 검색

```bash
history | fzh-exe
```

### 4. 디렉토리 이동

```bash
# 디렉토리 선택 후 이동
cd $(find . -type d | fzh-exe)
```

## 키 바인딩

| 키 | 동작 |
|---|---|
| 타이핑 | 실시간 필터링 |
| ↑ / Ctrl-K | 위로 이동 |
| ↓ / Ctrl-J | 아래로 이동 |
| Enter | 선택하고 종료 |
| Esc | 선택 없이 종료 |
| Ctrl-C | 강제 종료 |

## 고급 사용법

### 1. 셸 함수로 통합

**Bash/Zsh**:

```bash
# ~/.bashrc 또는 ~/.zshrc에 추가

# 파일 검색 후 에디터로 열기
fe() {
  local file
  file=$(find . -type f | fzh-exe)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# 디렉토리 검색 후 이동
fd() {
  local dir
  dir=$(find . -type d | fzh-exe)
  [ -n "$dir" ] && cd "$dir"
}

# Git 브랜치 전환
gb() {
  local branch
  branch=$(git branch --all | grep -v HEAD | fzh-exe)
  [ -n "$branch" ] && git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}
```

### 2. 파이프라인 활용

```bash
# 파일 검색 후 삭제
find . -name "*.tmp" | fzh-exe | xargs rm

# 로그 파일 검색 후 tail
find /var/log -name "*.log" | fzh-exe | xargs tail -f

# 파일 검색 후 grep
find . -name "*.hs" | fzh-exe | xargs grep "main"
```

### 3. 대용량 입력 처리

```bash
# 수천 개의 파일도 빠르게 처리
find / -type f 2>/dev/null | fzh-exe

# 긴 히스토리도 문제없음
cat ~/.bash_history | fzh-exe
```

## 성능 팁

### 1. 병렬 처리 활용

fzh는 자동으로 CPU 코어 수를 감지하여 병렬 처리합니다:

```bash
# RTS 옵션으로 코어 수 지정
fzh-exe +RTS -N4 -RTS < input.txt
```

### 2. 캐싱 효과

점진적으로 타이핑하면 캐시가 활용되어 더 빠릅니다:
- "te" → "tes" → "test" (각 단계에서 이전 결과 재사용)

### 3. 입력 최적화

```bash
# 불필요한 파일 제외
find . -type f -not -path "*/node_modules/*" | fzh-exe

# 특정 확장자만
find . -type f \( -name "*.hs" -o -name "*.cabal" \) | fzh-exe
```

## 벤치마크

성능 측정:

```bash
# 벤치마크 실행
stack bench

# 특정 항목 수로 테스트
time find . -type f | fzh-exe
```

## 문제 해결

### 1. 입력이 없다는 메시지

```bash
# 잘못된 사용
fzh-exe

# 올바른 사용 (파이프로 입력 전달)
echo "test" | fzh-exe
```

### 2. 한글/유니코드 문제

fzh는 UTF-8을 완전히 지원합니다:

```bash
# 한글 파일명도 검색 가능
find . -type f | fzh-exe
```

### 3. 성능 문제

```bash
# 더 많은 코어 사용
fzh-exe +RTS -N -RTS < input.txt

# 메모리 할당 증가
fzh-exe +RTS -A256m -RTS < input.txt
```

## 예제 스크립트

### 프로젝트 파일 검색기

```bash
#!/bin/bash
# project-search.sh

# 프로젝트 루트로 이동
cd "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

# 소스 파일만 검색
file=$(find . -type f \
  -not -path "*/.*" \
  -not -path "*/node_modules/*" \
  -not -path "*/.stack-work/*" \
  | fzh-exe)

# 선택된 파일 열기
[ -n "$file" ] && ${EDITOR:-vim} "$file"
```

### Git 커밋 브라우저

```bash
#!/bin/bash
# git-commit-browser.sh

commit=$(git log --oneline --color=always | fzh-exe | awk '{print $1}')
[ -n "$commit" ] && git show "$commit"
```

## 비교: fzf vs fzh

| 기능 | fzf | fzh |
|---|---|---|
| 언어 | Go | Haskell |
| 병렬 처리 | ✓ | ✓ |
| 캐싱 | ✓ | ✓ (STM) |
| 퍼지 매칭 | ✓ | ✓ |
| 미리보기 | ✓ | ✗ (향후 추가) |
| 멀티 선택 | ✓ | ✗ (향후 추가) |
| 타입 안정성 | - | ✓ (Haskell) |

## 추가 자료

- [아키텍처 문서](ARCHITECTURE.md)
- [벤치마크 결과](bench/)
- [예제 스크립트](examples/)

## 기여

버그 리포트나 기능 제안은 GitHub Issues를 이용해주세요.

## 라이선스

BSD-3-Clause
