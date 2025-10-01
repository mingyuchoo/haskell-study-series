# sqlite-simple-init

SQLite와 Haskell을 사용한 Clean Architecture 기반 REST API 서버

## 특징

- ✅ **Clean Architecture**: 비즈니스 로직을 외부 프레임워크로부터 독립적으로 유지
- ✅ **자동 초기화**: 서버 시작 시 데이터베이스 테이블 및 샘플 데이터 자동 생성
- ✅ **멀티 스테이지 Docker 빌드**: 빠른 빌드와 경량 이미지
- ✅ **RESTful API**: 표준 HTTP 메서드를 사용한 CRUD 작업
- ✅ **타입 안전성**: Haskell의 강력한 타입 시스템 활용

## 아키텍처

프로젝트는 Clean Architecture 원칙을 따릅니다:

```text
Domain (엔티티) → Application (유스케이스) → Adapters (인터페이스) → Infrastructure (외부 시스템)
```

자세한 내용은 [ARCHITECTURE.md](ARCHITECTURE.md)를 참조하세요.

## 빠른 시작

### 로컬 실행

```bash
# 빌드 및 실행
stack build
stack exec sqlite-simple-init-exe

# 또는 Makefile 사용
make run
```

서버가 시작되면:

- 📊 데이터베이스 테이블이 자동으로 생성됩니다
- 👥 5명의 샘플 사용자가 자동으로 추가됩니다
- 🚀 포트 3000에서 API 서버가 실행됩니다

### Docker 실행

```bash
# 멀티 스테이지 빌드 (권장)
make docker-build-multi
make docker-run-multi

# 또는 직접 실행
cd docker
docker build -t sqlite-simple-init:latest -f Dockerfile ..
docker run -p 8000:8000 sqlite-simple-init:latest
```

## REST API Usage

The server runs on `http://localhost:8000` and provides the following endpoints:

### List all users

```http
GET /users

Response: 200 OK
[{
  "userId": 1,
  "userName": "John Doe",
  "userEmail": "john@example.com",
  "userPassword": "password123",
  "createdAt": "2025-04-13T04:01:13Z",
  "updatedAt": "2025-04-13T04:01:13Z"
}]
```

### Get user by ID

```http
GET /users/:id

Response: 200 OK
{
  "userId": 1,
  "userName": "John Doe",
  "userEmail": "john@example.com",
  "userPassword": "password123",
  "createdAt": "2025-04-13T04:01:13Z",
  "updatedAt": "2025-04-13T04:01:13Z"
}

Response: 404 Not Found (if user not found)
```

### Create new user

```http
POST /users
Content-Type: application/x-www-form-urlencoded

Parameters:
- name: User's name
- email: User's email (must be unique)
- password: User's password

Response: 201 Created
{
  "userId": 1,
  "userName": "John Doe",
  "userEmail": "john@example.com",
  "userPassword": "password123",
  "createdAt": "2025-04-13T04:01:13Z",
  "updatedAt": "2025-04-13T04:01:13Z"
}
```

### Update user

```http
PUT /users/:id
Content-Type: application/x-www-form-urlencoded

Parameters:
- name: New name
- email: New email
- password: New password

Response: 200 OK (if updated)
Response: 404 Not Found (if user not found)
```

### Delete user

```http
DELETE /users/:id

Response: 204 No Content (if deleted)
Response: 404 Not Found (if user not found)
```

### Example using curl

```bash
# Create a new user
curl -X POST http://localhost:8000/users \
     -d "name=John Doe" \
     -d "email=john@example.com" \
     -d "password=password123"

# Get all users
curl http://localhost:8000/users

# Get user by ID
curl http://localhost:8000/users/1

# Update user
curl -X PUT http://localhost:8000/users/1 \
     -d "name=John Updated" \
     -d "email=john.updated@example.com" \
     -d "password=newpassword123"

# Delete user
curl -X DELETE http://localhost:8000/users/1
