# fzh 빠른 시작 가이드

## 5분 안에 시작하기

### 1. 빌드 (1분)

```bash
cd fzh
stack build
```

### 2. 첫 실행 (30초)

```bash
# 현재 디렉토리의 Haskell 파일 검색
find . -name "*.hs" | stack exec fzh-exe
```

### 3. 사용법 (1분)

1. **타이핑**: 검색어 입력 (예: "main")
2. **↑/↓**: 결과 탐색
3. **Enter**: 선택
4. **Esc**: 취소

### 4. 실용적인 예제 (2분)

#### 파일 검색 후 편집

```bash
# 파일 선택 후 vim으로 열기
vim $(find . -type f | stack exec fzh-exe)
```

#### Git 파일 검색

```bash
# Git 추적 파일만 검색
git ls-files | stack exec fzh-exe
```

#### 디렉토리 이동

```bash
# 디렉토리 선택 후 이동
cd $(find . -type d | stack exec fzh-exe)
```

## 다음 단계

- **상세 사용법**: [USAGE.md](USAGE.md)
- **기능 설명**: [FEATURES.md](FEATURES.md)
- **아키텍처**: [ARCHITECTURE.md](ARCHITECTURE.md)

## 문제 해결

### "No input provided" 오류

```bash
# 잘못된 사용
fzh-exe

# 올바른 사용 (파이프로 입력 전달)
echo "test" | fzh-exe
```

### 성능 최적화

```bash
# 더 많은 CPU 코어 사용
fzh-exe +RTS -N -RTS < input.txt
```

## 도움말

```bash
stack exec fzh-exe -- --help
stack exec fzh-exe -- --version
```

## 벤치마크

```bash
stack bench
```

## 테스트

```bash
stack test
```

## 시스템 설치

```bash
make install
# ~/.local/bin/fzh-exe에 설치됨
```

이제 `fzh-exe`를 어디서든 사용할 수 있습니다!
