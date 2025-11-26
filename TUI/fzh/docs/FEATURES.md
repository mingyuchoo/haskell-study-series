# fzh 기능 명세

## 구현된 핵심 기능

### 1. 퍼지 매칭 (Fuzzy Matching)

**구현 위치**: `src/FuzzySearch.hs`

**특징**:
- fzf 스타일의 퍼지 매칭 알고리즘
- 스코어링 시스템으로 관련성 높은 결과 우선 표시
- 보너스 점수 시스템:
  - 시작 위치 매칭: +10점
  - 단어 경계 매칭: +8점
  - 일반 매칭: +1점

**예제**:
```haskell
-- "test"로 "test_file_123.txt" 검색
fuzzyMatch "test" "test_file_123.txt"
-- Just (FuzzyResult {frScore = 14, frMatches = [0,1,2,3]})

-- "tf"로 "test_file.txt" 검색
fuzzyMatch "tf" "test_file.txt"
-- Just (FuzzyResult {frScore = 18, frMatches = [0,5]})
```

### 2. 병렬 검색 (Parallel Search)

**구현 위치**: `src/ParallelFilter.hs`

**특징**:
- `async` 라이브러리를 이용한 멀티코어 활용
- CPU 코어 수에 따른 자동 작업 분할
- 각 청크는 독립적으로 병렬 처리
- 결과는 점수 순으로 자동 정렬

**성능**:
```
단일 코어: O(n)
멀티 코어: O(n/k) where k = 코어 수
```

**예제**:
```haskell
-- 10000개 항목을 4코어에서 병렬 처리
items <- readInputLines
results <- parallelFilter "query" items
-- 각 코어가 2500개씩 처리
```

### 3. 메모이제이션과 캐싱 (Memoization & Caching)

**구현 위치**: `src/Cache.hs`

**특징**:
- STM (Software Transactional Memory) 기반
- 트랜잭셔널 메모리로 안전한 동시성
- 락 프리 알고리즘
- 점진적 타이핑 시 이전 결과 재사용

**캐시 전략**:
```
"t" 검색 → 결과 캐싱
"te" 검색 → "t" 결과 재사용 (캐시 히트)
"tes" 검색 → "te" 결과 재사용 (캐시 히트)
```

**예제**:
```haskell
cache <- newCache
-- 첫 검색 (캐시 미스)
results1 <- parallelFilter "test" items
insert "test" results1 cache

-- 같은 쿼리 재검색 (캐시 히트)
cached <- lookup "test" cache
-- Just results1 (즉시 반환)
```

### 4. 터미널 렌더링 (Terminal Rendering)

**구현 위치**: `src/UI.hs`

**특징**:
- `brick` 라이브러리 기반 TUI
- 실시간 필터링 결과 표시
- 키보드 네비게이션
- 선택된 항목 하이라이팅

**UI 구성**:
```
┌─────────────────────────────────┐
│ FZF in Haskell - Fuzzy Finder   │
├─────────────────────────────────┤
│ Query                           │
│ > test_                         │
├─────────────────────────────────┤
│ Results                         │
│ ▶ test_file_1.txt      14       │
│   test_file_2.txt      14       │
│   test_main.hs         12       │
├─────────────────────────────────┤
│ 3/100                           │
└─────────────────────────────────┘
```

### 5. 점진적 필터링 (Incremental Filtering)

**구현 방식**:
- 사용자가 타이핑할 때마다 실시간 필터링
- 캐시를 활용하여 빠른 응답
- 이전 결과를 기반으로 점진적 업데이트

**흐름**:
```
사용자 입력 → 쿼리 변경 감지
    ↓
캐시 조회
    ↓
캐시 히트? → 즉시 결과 반환
    ↓ (미스)
병렬 필터링 수행
    ↓
결과 캐싱 및 UI 업데이트
```

### 6. 스트리밍 입력 (Streaming Input)

**구현 위치**: `src/InputReader.hs`

**특징**:
- `streaming` 라이브러리 사용
- 대용량 입력도 메모리 효율적으로 처리
- 지연 평가로 필요한 만큼만 읽기

**두 가지 모드**:
1. **일괄 읽기**: 모든 입력을 Vector로 로드
2. **스트리밍**: 필요할 때마다 읽기 (향후 활용)

## 기술 스택 활용

### async
```haskell
-- 병렬 필터링
results <- mapConcurrently (filterChunk query) chunks
```

### stm
```haskell
-- 트랜잭셔널 캐시 업데이트
atomically $ modifyTVar' tvar (M.insert key value)
```

### Control.Concurrent
```haskell
-- CPU 코어 수 감지
numCaps <- getNumCapabilities
```

### text
```haskell
-- 효율적인 텍스트 처리
queryLower = T.toLower query
targetLower = T.toLower target
```

### vector
```haskell
-- 고성능 배열 연산
items <- V.fromList inputList
V.foldr' processItem [] chunk
```

### criterion
```haskell
-- 성능 벤치마킹
bench "1000 items" $ nfIO $ parallelFilter query items
```

### streaming
```haskell
-- 스트리밍 입력 처리
streamInputLines :: IO (Stream (Of T.Text) IO ())
```

### brick
```haskell
-- 터미널 UI
drawUI :: AppState -> [Widget Name]
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
```

## 성능 특성

### 시간 복잡도

| 연산 | 복잡도 | 설명 |
|---|---|---|
| 퍼지 매칭 | O(n*m) | n=쿼리 길이, m=대상 길이 |
| 병렬 필터링 | O(n/k) | n=항목 수, k=코어 수 |
| 캐시 조회 | O(log n) | Map 기반 |
| 결과 정렬 | O(n log n) | 점수 기반 정렬 |

### 공간 복잡도

| 자료구조 | 복잡도 | 설명 |
|---|---|---|
| 입력 Vector | O(n) | 전체 입력 저장 |
| 캐시 Map | O(k) | k=캐시된 쿼리 수 |
| 결과 리스트 | O(m) | m=매칭된 항목 수 |

## 확장 가능성

### 단기 개선 사항

1. **정규식 지원**
   ```haskell
   regexMatch :: Regex -> Text -> Maybe MatchResult
   ```

2. **대소문자 구분 옵션**
   ```haskell
   data MatchMode = CaseSensitive | CaseInsensitive
   ```

3. **멀티 선택**
   ```haskell
   data AppState = AppState
     { ...
     , selectedItems :: Set Int
     }
   ```

### 장기 개선 사항

1. **인덱싱**
   - 트라이 기반 인덱스
   - 접두사 검색 최적화

2. **GPU 가속**
   - CUDA를 이용한 병렬 매칭
   - 대규모 데이터셋 처리

3. **플러그인 시스템**
   - 커스텀 매칭 알고리즘
   - 외부 도구 통합

## 테스트 커버리지

### 단위 테스트
- [ ] FuzzySearch 모듈
- [ ] ParallelFilter 모듈
- [ ] Cache 모듈

### 통합 테스트
- [x] 전체 파이프라인
- [x] UI 인터랙션

### 벤치마크
- [x] 퍼지 매칭 성능
- [x] 병렬 필터링 성능
- [ ] 캐시 효율성

## 알려진 제한사항

1. **미리보기 기능 없음**
   - fzf의 `--preview` 옵션 미지원
   - 향후 추가 예정

2. **멀티 선택 미지원**
   - 현재는 단일 선택만 가능
   - Tab 키로 멀티 선택 추가 예정

3. **설정 파일 없음**
   - 현재는 하드코딩된 설정
   - YAML 설정 파일 지원 예정

## 라이선스

BSD-3-Clause
