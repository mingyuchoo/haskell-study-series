# fzh 아키텍처 문서

## 개요

fzh는 Haskell로 구현한 fzf 스타일의 퍼지 파인더입니다. 함수형 프로그래밍의 장점을 활용하여 높은 성능과 안정성을 제공합니다.

## 핵심 모듈

### 1. FuzzySearch (src/FuzzySearch.hs)

**목적**: 퍼지 매칭 알고리즘 구현

**주요 기능**:
- `fuzzyMatch`: 쿼리와 대상 텍스트 간의 퍼지 매칭 수행
- `fuzzyScore`: 매칭 점수 계산
- 보너스 점수: 단어 경계, 시작 위치 등에 가중치 부여

**최적화**:
- BangPatterns를 사용한 strict 평가로 메모리 누수 방지
- Text 타입 사용으로 효율적인 문자열 처리
- 인덱스 기반 순회로 불필요한 할당 최소화

### 2. ParallelFilter (src/ParallelFilter.hs)

**목적**: 병렬 필터링 엔진

**주요 기능**:
- `parallelFilter`: 입력을 청크로 나누어 병렬 처리
- `splitVector`: CPU 코어 수에 따라 작업 분할
- `filterChunk`: 각 청크에 대한 필터링 수행

**병렬화 전략**:
```haskell
-- CPU 코어 수만큼 청크 생성
numCaps <- getNumCapabilities
let chunks = splitVector numCaps items

-- async를 이용한 병렬 실행
results <- mapConcurrently (filterChunk query) chunks
```

**성능 특성**:
- O(n/k) 시간 복잡도 (k = 코어 수)
- 각 청크는 독립적으로 처리되어 동기화 오버헤드 최소화

### 3. Cache (src/Cache.hs)

**목적**: STM 기반 결과 캐싱

**주요 기능**:
- `newCache`: 새 캐시 생성
- `lookup`: 캐시에서 결과 조회
- `insert`: 결과를 캐시에 저장
- `clear`: 캐시 초기화

**STM 사용 이유**:
- 트랜잭셔널 메모리로 안전한 동시성 보장
- 락 프리 알고리즘으로 높은 처리량
- 데드락 없는 설계

```haskell
-- STM을 이용한 안전한 캐시 업데이트
insert key value (Cache tvar) = atomically $ do
    modifyTVar' tvar (M.insert key value)
```

### 4. InputReader (src/InputReader.hs)

**목적**: 효율적인 입력 처리

**주요 기능**:
- `readInputLines`: 모든 입력을 Vector로 읽기
- `streamInputLines`: 스트리밍 방식으로 입력 처리

**메모리 효율성**:
- Vector 사용으로 연속 메모리 할당
- 스트리밍 지원으로 대용량 입력 처리 가능

### 5. AppState (src/AppState.hs)

**목적**: 애플리케이션 상태 관리

**주요 컴포넌트**:
- `queryEditor`: 쿼리 입력 에디터
- `resultList`: 필터링 결과 리스트
- `allItems`: 전체 입력 항목
- `currentQuery`: 현재 쿼리 문자열

**렌즈 사용**:
```haskell
makeLenses ''AppState

-- 렌즈를 이용한 불변 업데이트
updateQuery query st = st & currentQuery .~ query
```

### 6. UI (src/UI.hs)

**목적**: Brick 기반 터미널 UI

**주요 기능**:
- `drawUI`: UI 렌더링
- `handleEvent`: 키보드 이벤트 처리
- `theApp`: Brick 애플리케이션 정의

**이벤트 처리 흐름**:
1. 사용자 입력 감지
2. 쿼리 변경 확인
3. 캐시 조회
4. 캐시 미스 시 병렬 필터링 수행
5. 결과 업데이트 및 UI 재렌더링

## 데이터 흐름

```
입력 (stdin)
    ↓
InputReader (Vector로 변환)
    ↓
AppState (초기 상태 생성)
    ↓
UI (Brick 앱 시작)
    ↓
사용자 입력 → 쿼리 변경
    ↓
Cache 조회
    ↓
캐시 히트? → 결과 반환
    ↓ (미스)
ParallelFilter (병렬 필터링)
    ↓
FuzzySearch (각 항목 매칭)
    ↓
결과 정렬 및 캐싱
    ↓
UI 업데이트
```

## 성능 최적화 기법

### 1. 병렬 처리
- `async` 라이브러리로 멀티코어 활용
- CPU 코어 수에 따른 동적 청크 분할
- 각 청크는 독립적으로 처리

### 2. 캐싱
- STM 기반 트랜잭셔널 캐시
- 이전 쿼리 결과 재사용
- 점진적 필터링 시 효율적

### 3. Strict 평가
- BangPatterns로 메모리 누수 방지
- 불필요한 thunk 생성 최소화

### 4. 효율적인 자료구조
- Vector: O(1) 인덱싱, 연속 메모리
- Text: 효율적인 유니코드 처리
- Map: O(log n) 캐시 조회

### 5. 스트리밍
- 대용량 입력 처리를 위한 streaming 지원
- 메모리 사용량 제한

## 벤치마크

벤치마크는 `criterion` 라이브러리를 사용합니다:

```bash
stack bench
```

**측정 항목**:
- 퍼지 매칭 속도
- 병렬 필터링 성능 (100, 1000, 10000 항목)
- 캐시 히트율

## 확장 가능성

### 추가 가능한 기능

1. **다양한 매칭 알고리즘**
   - 정규식 지원
   - 대소문자 구분 옵션
   - 정확한 매칭 모드

2. **UI 개선**
   - 미리보기 창
   - 멀티 선택
   - 색상 하이라이팅

3. **성능 향상**
   - 인덱싱 기반 검색
   - 증분 업데이트
   - GPU 가속 (CUDA)

4. **통합**
   - 셸 통합 (zsh, bash)
   - 에디터 플러그인 (vim, emacs)
   - 파일 시스템 워처

## 의존성

### 핵심 라이브러리

- **async**: 비동기 병렬 처리
- **stm**: 트랜잭셔널 메모리
- **brick**: 터미널 UI 프레임워크
- **text**: 효율적인 텍스트 처리
- **vector**: 고성능 배열
- **streaming**: 스트림 처리
- **criterion**: 벤치마킹

### 빌드 도구

- **stack**: Haskell 빌드 도구
- **GHC 9.10.3**: Haskell 컴파일러

## 테스트

```bash
# 단위 테스트
stack test

# 벤치마크
stack bench

# 통합 테스트
./test-fzh.sh
```

## 라이선스

BSD-3-Clause
