# Frontend-Backend 통합 배포 요약

## 완료된 작업

### 1. 빌드 시스템 구성
- ✅ Makefile에 frontend 빌드 통합
- ✅ `frontend-build` 타겟: React 앱 빌드 및 backend/static/로 복사
- ✅ `frontend-clean` 타겟: 빌드 아티팩트 정리

### 2. Backend 설정
- ✅ `backend/package.yaml`에 `data-files: static/**/*` 추가
- ✅ `backend/.gitignore`에 `static/` 추가
- ✅ `backend/src/Presentation/API.hs`에서 `serveDirectoryFileServer "static"` 설정
- ✅ `backend/src/Presentation/Server.hs`에 Web UI 로그 메시지 추가

### 3. Frontend 설정
- ✅ `vite.config.ts`에 proxy 설정 (개발 모드용)
- ✅ `base: '/'` 설정으로 루트 경로 서빙
- ✅ 빌드 출력: `frontend/dist/`

### 4. 통합 테스트
- ✅ Frontend 빌드 성공
- ✅ Backend 빌드 성공
- ✅ Static 파일 복사 확인
- ✅ 통합 테스트 스크립트 작성

## 디렉터리 구조

```
azure-openai-haskell-agent/
├── frontend/
│   ├── src/              # React 소스 코드
│   ├── dist/             # 빌드 출력 (생성됨)
│   ├── vite.config.ts    # Vite 설정 (proxy 포함)
│   └── package.json
│
├── backend/
│   ├── src/
│   │   └── Presentation/
│   │       ├── API.hs    # Static 파일 서빙 설정
│   │       └── Server.hs # 서버 시작 및 로그
│   ├── static/           # Frontend 빌드 복사본 (생성됨)
│   │   ├── index.html
│   │   ├── assets/
│   │   └── vite.svg
│   ├── package.yaml      # data-files 설정
│   └── .gitignore        # static/ 제외
│
├── Makefile              # 빌드 오케스트레이션
└── test-integration.sh   # 통합 테스트 스크립트
```

## 빌드 프로세스

```bash
make build
```

실행 순서:
1. `frontend-build` 실행
   - `cd frontend && pnpm install`
   - `cd frontend && pnpm run build`
   - `mkdir -p backend/static`
   - `cp -r frontend/dist/* backend/static/`
2. `stack build` 실행 (backend)

## 실행 방법

### Production 모드 (통합)
```bash
make build  # 전체 빌드
make run    # 서버 실행
```

서버 접속:
- Web UI: http://localhost:8000/
- API: http://localhost:8000/api/chat
- Health: http://localhost:8000/health
- Swagger UI: http://localhost:8000/swagger-ui

### Development 모드 (분리)

**Terminal 1 - Backend:**
```bash
cd backend
stack build
stack exec backend-exe
```

**Terminal 2 - Frontend (Hot Reload):**
```bash
cd frontend
pnpm run dev
```

Frontend 접속: http://localhost:5173/ (API는 :8000으로 프록시)

## 통합 확인

```bash
./test-integration.sh
```

이 스크립트는 다음을 확인합니다:
- Frontend 빌드 파일 존재 여부
- Backend 실행 파일 존재 여부
- 환경 설정 파일 존재 여부

## API 엔드포인트

### Static Files (Frontend)
- `GET /` → `backend/static/index.html`
- `GET /assets/*` → `backend/static/assets/*`

### API Routes
- `POST /api/chat` → Chat 요청 처리
- `GET /health` → Health check

### Documentation
- `GET /swagger-ui` → Swagger UI
- `GET /openapi.json` → OpenAPI 스펙

## 주요 파일 변경사항

### backend/package.yaml
```yaml
data-files:
- static/**/*
```

### backend/.gitignore
```
static/
```

### backend/src/Presentation/API.hs
```haskell
type API = ChatAPI
      :<|> SwaggerSchemaUI "swagger-ui" "openapi.json"
      :<|> Raw

server :: ChatConfig -> Server API
server config = (chatHandler :<|> healthHandler)
           :<|> swaggerSchemaUIServer swaggerDoc
           :<|> serveDirectoryFileServer "static"
```

### frontend/vite.config.ts
```typescript
export default defineConfig({
  base: '/',
  build: {
    outDir: 'dist',
  },
  server: {
    proxy: {
      '/api': 'http://localhost:8000',
      '/health': 'http://localhost:8000',
    }
  }
})
```

## 문제 해결

### Frontend가 표시되지 않는 경우
```bash
# 1. Static 파일 확인
ls -la backend/static/

# 2. Frontend 재빌드
make frontend-clean
make frontend-build

# 3. Backend 재빌드
cd backend && stack clean && stack build
```

### API 호출 실패
- Backend가 8000 포트에서 실행 중인지 확인
- CORS 설정 확인 (이미 설정됨)
- 브라우저 콘솔에서 에러 확인

### 빌드 실패
```bash
# 전체 클린 후 재빌드
make clean
make build
```

## 다음 단계

1. ✅ Frontend-Backend 통합 완료
2. 🔄 Frontend에서 실제 API 호출 구현
3. 🔄 Docker 이미지에 frontend 빌드 포함
4. 🔄 CI/CD 파이프라인 구성
5. 🔄 Production 환경 배포

## 참고 문서

- [INTEGRATION.md](./INTEGRATION.md) - 상세 통합 가이드
- [FRONTEND_INTEGRATION.md](./FRONTEND_INTEGRATION.md) - Frontend 통합 세부사항
- [backend/README.md](../backend/README.md) - Backend 문서
- [frontend/README.md](../frontend/README.md) - Frontend 문서
