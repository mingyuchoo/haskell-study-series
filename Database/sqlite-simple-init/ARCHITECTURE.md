# Clean Architecture 구조

이 프로젝트는 Clean Architecture 원칙에 따라 구성되어 있으며, 비즈니스 로직을 외부 프레임워크, 데이터베이스, UI로부터 독립적으로 유지합니다.

## 디렉터리 구조

```
src/
├── Domain/                      # 도메인 계층 (가장 내부)
│   └── UserModel.hs            # 엔티티 및 비즈니스 규칙
│
├── Application/                 # 애플리케이션 계층
│   └── UserService.hs          # 유스케이스 인터페이스
│
├── Adapters/                    # 어댑터 계층 (인터페이스 어댑터)
│   ├── Repository/
│   │   └── UserRepositoryAdapter.hs  # 저장소 구현
│   └── Web/
│       └── UserWebAdapter.hs         # 웹 API 구현
│
├── Infrastructure/              # 인프라 계층 (가장 외부)
│   ├── Config/
│   │   └── AppConfig.hs        # 설정 관리
│   ├── Database/
│   │   ├── Connection.hs       # DB 연결 관리
│   │   └── SampleData.hs       # 샘플 데이터 초기화
│   ├── Repository/             # (레거시 - 마이그레이션 중)
│   │   └── UserRepository.hs
│   └── Web/                    # (레거시 - 마이그레이션 중)
│       └── Server.hs
│
└── Lib.hs                       # 애플리케이션 조합 및 진입점
```

## 계층별 설명

### 1. Domain (도메인 계층)
**책임**: 핵심 비즈니스 로직과 엔티티

- `UserModel.hs`: User 엔티티 정의
- 외부 의존성 없음 (순수 비즈니스 로직)
- 다른 계층에 의존하지 않음

**원칙**:
- 프레임워크 독립적
- 테스트 가능
- 변경에 가장 안정적

### 2. Application (애플리케이션 계층)
**책임**: 유스케이스 정의 및 비즈니스 흐름 조율

- `UserService.hs`: 사용자 관련 유스케이스 인터페이스
  - `createUser`: 사용자 생성
  - `getAllUsers`: 모든 사용자 조회
  - `getUserById`: ID로 사용자 조회
  - `updateUser`: 사용자 정보 업데이트
  - `deleteUser`: 사용자 삭제

**원칙**:
- Domain 계층만 의존
- 구체적인 구현에 의존하지 않음 (인터페이스만 정의)

### 3. Adapters (어댑터 계층)
**책임**: 외부 세계와 애플리케이션 계층 사이의 변환

#### Repository Adapter
- `UserRepositoryAdapter.hs`: SQLite 데이터베이스 어댑터
- `UserService` 인터페이스 구현
- 데이터베이스 접근 로직 캡슐화

#### Web Adapter
- `UserWebAdapter.hs`: HTTP/REST API 어댑터
- Scotty 웹 프레임워크 사용
- API 엔드포인트 정의:
  - `GET /users`: 모든 사용자 조회
  - `GET /users/:id`: 특정 사용자 조회
  - `POST /users`: 새 사용자 생성
  - `PUT /users/:id`: 사용자 정보 업데이트
  - `DELETE /users/:id`: 사용자 삭제

**원칙**:
- Application 계층의 인터페이스 구현
- 외부 프레임워크/라이브러리 의존성 격리

### 4. Infrastructure (인프라 계층)
**책임**: 외부 시스템과의 통합 및 설정

#### Config
- `AppConfig.hs`: 애플리케이션 설정
  - 포트 번호
  - 데이터베이스 경로
  - 샘플 데이터 활성화 여부

#### Database
- `Connection.hs`: 데이터베이스 연결 관리
  - 데이터베이스 초기화
  - 테이블 생성
- `SampleData.hs`: 초기 샘플 데이터 생성
  - 서버 시작 시 자동 실행
  - 5명의 샘플 사용자 생성

**원칙**:
- 가장 외부 계층
- 구체적인 기술 구현
- 다른 모든 계층에 의존 가능

### 5. Lib.hs (조합 루트)
**책임**: 모든 계층을 조합하여 애플리케이션 구성

```haskell
startApp :: IO ()
startApp = do
    -- 1. 설정 로드
    let config = defaultConfig
    
    -- 2. 데이터베이스 초기화
    conn <- initializeDatabase
    
    -- 3. 어댑터 생성
    let repository = createUserRepositoryAdapter conn
    
    -- 4. 샘플 데이터 삽입
    insertSampleData repository
    
    -- 5. 웹 서버 시작
    startWebServer repository
```

## 의존성 규칙

```
Domain (가장 내부)
  ↑
Application
  ↑
Adapters
  ↑
Infrastructure (가장 외부)
```

- 의존성은 항상 안쪽(내부)을 향함
- 내부 계층은 외부 계층을 알지 못함
- 외부 계층은 내부 계층의 인터페이스에만 의존

## 장점

### 1. 테스트 용이성
- 각 계층을 독립적으로 테스트 가능
- Mock 객체로 외부 의존성 대체 가능

### 2. 유지보수성
- 관심사의 명확한 분리
- 변경의 영향 범위 최소화

### 3. 독립성
- **프레임워크 독립성**: Scotty를 다른 웹 프레임워크로 교체 가능
- **데이터베이스 독립성**: SQLite를 PostgreSQL로 교체 가능
- **UI 독립성**: REST API를 GraphQL로 교체 가능

### 4. 비즈니스 로직 보호
- 핵심 비즈니스 로직이 외부 변경에 영향받지 않음
- 기술 스택 변경이 비즈니스 로직에 영향 없음

## 실행 방법

### 로컬 개발
```bash
stack build
stack exec sqlite-simple-init-exe
```

서버가 시작되면:
- 데이터베이스 테이블 자동 생성
- 5명의 샘플 사용자 자동 생성
- 포트 3000에서 API 서버 실행

### Docker 배포
```bash
cd docker
docker build -t sqlite-simple-init:latest -f Dockerfile ..
docker run -p 8000:8000 sqlite-simple-init:latest
```

## API 테스트

```bash
# 모든 사용자 조회
curl http://localhost:8000/users

# 특정 사용자 조회
curl http://localhost:8000/users/1

# 새 사용자 생성
curl -X POST http://localhost:8000/users \
  -d "name=테스트&email=test@example.com&password=test123"

# 사용자 정보 업데이트
curl -X PUT http://localhost:8000/users/1 \
  -d "name=수정된이름&email=updated@example.com&password=newpass"

# 사용자 삭제
curl -X DELETE http://localhost:8000/users/1
```

## 향후 개선 사항

1. **레거시 코드 제거**
   - `Infrastructure/Repository/UserRepository.hs` → 삭제 예정
   - `Infrastructure/Web/Server.hs` → 삭제 예정

2. **테스트 추가**
   - 단위 테스트 (각 계층별)
   - 통합 테스트
   - E2E 테스트

3. **추가 기능**
   - 인증/인가 (JWT)
   - 로깅
   - 에러 핸들링 개선
   - 입력 검증

4. **성능 최적화**
   - 연결 풀링
   - 캐싱
   - 비동기 처리
