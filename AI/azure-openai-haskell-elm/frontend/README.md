# OpenAI Chat Assistant - Elm Frontend

React/TypeScript 프론트엔드를 Elm으로 포팅한 버전입니다.

## 필수 요구사항

- [Elm](https://guide.elm-lang.org/install/elm.html) 0.19.1 이상

## 설치

```bash
# Elm 설치 (아직 설치하지 않은 경우)
bun install -g elm

# 프로젝트 디렉토리로 이동
cd frontend

# Elm 패키지 설치
elm make src/Main.elm
```

## 개발

```bash
# Elm 애플리케이션 빌드
elm make src/Main.elm --output=public/elm.js

# 개발 서버 실행 (간단한 HTTP 서버 필요)
# Python 3를 사용하는 경우:
cd public
python3 -m http.server 8080

# 또는 Node.js http-server를 사용하는 경우:
npx http-server public -p 8080
```

브라우저에서 `http://localhost:8080`을 열어 애플리케이션을 확인하세요.

## 프로덕션 빌드

```bash
# 최적화된 빌드
elm make src/Main.elm --output=public/elm.js --optimize
```

## 프로젝트 구조

```
frontend/
├── elm.json              # Elm 패키지 설정
├── src/
│   └── Main.elm         # 메인 애플리케이션 (Model, Update, View)
├── public/
│   └── index.html       # HTML 엔트리포인트 (CSS 포함)
└── README.md
```

## 주요 기능

- ✅ 채팅 메시지 송수신
- ✅ 로딩 상태 표시
- ✅ 에러 처리
- ✅ Enter 키로 메시지 전송
- ✅ 채팅 초기화
- ✅ 반응형 UI

## API 설정

`public/index.html` 파일에서 API URL을 변경할 수 있습니다:

```javascript
const apiUrl = 'http://localhost:8000';  // 백엔드 API URL
```

## Elm 아키텍처

이 애플리케이션은 Elm 아키텍처를 따릅니다:

- **Model**: 애플리케이션 상태 (메시지 목록, 입력값, 로딩 상태 등)
- **Update**: 상태 변경 로직 (메시지 전송, 응답 처리 등)
- **View**: UI 렌더링 (HTML 생성)

## 원본 프로젝트와의 차이점

- React Hooks 대신 Elm 아키텍처 사용
- TypeScript 대신 Elm의 강력한 타입 시스템 사용
- 런타임 에러가 없는 순수 함수형 프로그래밍
- 컴파일 타임에 모든 에러 검출
