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
ARG PROJECT_NAME=postgresql-simple-init
RUN stack build --no-docker --copy-bins --local-bin-path /build/bin

# 실행 파일 이름 표준화
RUN cp /build/bin/${PROJECT_NAME}-exe /build/bin/app-exe

# 필요한 공유 라이브러리 수집
# - ldd로 실행 파일이 참조하는 .so를 확인 후, Stack/GHC 빌드 경로의 것들만 모읍니다.
# - 수집 위치: /build/lib
RUN set -eux; \
    mkdir -p /build/lib; \
    ldd /build/bin/app-exe | awk '{print $3}' | grep -E '^/root/.stack|^/build/.stack-work' | while read -r so; do \
      if [ -f "$so" ]; then \
        cp -v --parents "$so" /build/lib || true; \
      fi; \
    done; \
    # 경로 평탄화: 하위 디렉터리 구조를 제거하고 순수 파일만 /build/lib 에 모읍니다
    find /build/lib -type f -name '*.so*' -exec bash -c 'for f; do base=$(basename "$f"); dest="/build/lib/$base"; [ -e "$dest" ] || mv -v "$f" "$dest"; done' _ {} +; \
    find /build/lib -type d -empty -delete || true

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
        netbase \
        locales && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# 빌드된 실행 파일만 복사
COPY --from=builder /build/bin/app-exe /app/

# Haskell/Stack 빌드에서 생성된 공유 라이브러리 복사 및 로더 경로 설정
COPY --from=builder /build/lib/ /app/lib/
RUN echo "/app/lib" > /etc/ld.so.conf.d/app.conf && ldconfig

# UTF-8 로케일 설정 (한글 출력 시 인코딩 오류 방지)
RUN sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8 \
    LANGUAGE=en_US:en

# 정적 파일 복사
COPY static/ /app/static/

# 데이터 디렉터리 생성
RUN mkdir -p /app/data

# 포트 노출
EXPOSE 8000

# 실행
CMD ["/app/app-exe"]
