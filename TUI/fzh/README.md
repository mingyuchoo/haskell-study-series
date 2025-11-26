# fzh - Fuzzy Finder in Haskell

Haskell로 구현한 fzf 스타일의 퍼지 파인더입니다.

## 주요 기능

- **퍼지 매칭**: fzf와 유사한 스코어링 알고리즘
- **병렬 검색**: `async`와 `Control.Concurrent`를 이용한 멀티코어 활용
- **점진적 필터링**: 타이핑하면서 실시간 결과 업데이트
- **캐싱**: STM 기반 메모이제이션으로 성능 최적화
- **터미널 UI**: brick 라이브러리를 이용한 인터랙티브 인터페이스
- **스트리밍**: 대용량 입력 처리를 위한 streaming 지원

## 핵심 기술 스택

- **async**: 비동기 병렬 처리
- **stm**: 트랜잭셔널 메모리 기반 캐싱
- **brick**: 터미널 UI 프레임워크
- **text**: 효율적인 텍스트 처리
- **vector**: 고성능 배열 연산
- **streaming**: 메모리 효율적인 스트림 처리
- **criterion**: 성능 벤치마킹

## 빠른 시작

### 설치

```bash
# 빌드
stack build

# 시스템에 설치
make install
```

### 기본 사용

```bash
# 파일 검색
find . -type f | stack exec fzh-exe

# Git 파일 검색
git ls-files | stack exec fzh-exe

# 프로세스 검색
ps aux | stack exec fzh-exe

# 도움말
stack exec fzh-exe -- --help
```

## 키 바인딩

| 키 | 동작 |
|---|---|
| 타이핑 | 실시간 필터링 |
| ↑/↓ | 결과 탐색 |
| Enter | 선택하고 종료 |
| Esc | 선택 없이 종료 |
| Ctrl-C | 강제 종료 |

## 문서

- **[사용 가이드](USAGE.md)**: 상세한 사용법과 예제
- **[기능 명세](FEATURES.md)**: 구현된 기능 상세 설명
- **[아키텍처](ARCHITECTURE.md)**: 내부 구조와 설계 원칙

## 프로젝트 구조

```
fzh/
├── src/
│   ├── FuzzySearch.hs      # 퍼지 매칭 알고리즘
│   ├── ParallelFilter.hs   # 병렬 필터링 엔진
│   ├── Cache.hs            # STM 기반 캐싱
│   ├── InputReader.hs      # 스트리밍 입력 처리
│   ├── AppState.hs         # 상태 관리
│   ├── UI.hs               # Brick UI
│   └── Lib.hs              # 메인 라이브러리
├── app/
│   └── Main.hs             # 실행 파일 엔트리
├── bench/
│   └── Benchmark.hs        # 성능 벤치마크
├── test/
│   └── Spec.hs             # 테스트
└── examples/
    └── demo.sh             # 데모 스크립트
```

## 개발

### 빌드

```bash
# 빠른 빌드
stack build --fast

# 최적화 빌드
stack build
```

### 테스트

```bash
# 단위 테스트
stack test

# 벤치마크
stack bench

# 통합 테스트
./test-fzh.sh
```

### 포맷팅

```bash
make format
```

## 성능

### 벤치마크 실행

```bash
stack bench
```

### 최적화 기법

1. **병렬 처리**: CPU 코어 수에 따른 자동 작업 분할
2. **캐싱**: STM 기반 트랜잭셔널 캐시로 이전 결과 재사용
3. **Strict 평가**: BangPatterns로 메모리 누수 방지
4. **효율적 자료구조**: Vector, Text, Map 활용

### 성능 팁

```bash
# 더 많은 코어 사용
stack exec fzh-exe -- +RTS -N -RTS < input.txt

# 메모리 할당 증가
stack exec fzh-exe -- +RTS -A256m -RTS < input.txt
```

## 예제

### 셸 통합

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
```

더 많은 예제는 [USAGE.md](USAGE.md)를 참고하세요.

## 기여

버그 리포트나 기능 제안은 GitHub Issues를 이용해주세요.

## 라이선스

BSD-3-Clause

## 작성자

Mingyu Choo (mingyuchoo@gmail.com)
