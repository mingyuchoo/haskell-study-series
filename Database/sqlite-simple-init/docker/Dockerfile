# ============================================
# 1단계: 의존성 캐시 레이어
# ============================================
FROM fpco/stack-build:lts-24.4 AS dependencies

WORKDIR /build

# Stack 설정 파일과 프로젝트 메타데이터 복사
COPY stack.yaml stack.yaml.lock ./
COPY package.yaml *.cabal Setup.hs ./
COPY README.md CHANGELOG.md LICENSE ./

# 의존성 다운로드 및 빌드 (캐시 활용)
RUN stack setup --no-docker && \
    stack build --only-dependencies --no-docker

# ============================================
# 2단계: 애플리케이션 빌드
# ============================================
FROM fpco/stack-build:lts-24.4 AS builder

WORKDIR /build

# 1단계에서 빌드한 의존성 복사
COPY --from=dependencies /root/.stack /root/.stack
COPY --from=dependencies /build/.stack-work /build/.stack-work

# 프로젝트 파일 복사
COPY stack.yaml stack.yaml.lock package.yaml *.cabal Setup.hs ./
COPY README.md CHANGELOG.md LICENSE ./

# 소스 코드 복사
COPY src/ src/
COPY app/ app/
COPY test/ test/

# 애플리케이션 빌드
ARG PROJECT_NAME=sqlite-simple-init
RUN stack build --no-docker --copy-bins --local-bin-path /build/bin

# 실행 파일 이름 표준화
RUN cp /build/bin/${PROJECT_NAME}-exe /build/bin/app-exe

# ============================================
# 3단계: 런타임 이미지 (경량화)
# ============================================
FROM ubuntu:22.04 AS runtime

# 필요한 런타임 라이브러리 설치
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        libgmp10 \
        libsqlite3-0 \
        netbase && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 빌드된 실행 파일만 복사
COPY --from=builder /build/bin/app-exe /app/

# 정적 파일 복사
COPY static/ /app/static/

# 데이터 디렉터리 생성
RUN mkdir -p /app/data

# 포트 노출
EXPOSE 8000

# 실행
CMD ["/app/app-exe"]
