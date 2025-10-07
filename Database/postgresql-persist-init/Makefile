.PHONY: all clean setup format build test coverage watch-test watch-coverage ghcid run run-cache run-esq migrate migrate-esq release docker-build docker-build-multi docker-run docker-run-multi docker-up docker-up-all docker-down docker-logs docker-build-app docker-restart-app docker-compose-up docker-compose-down docker-compose-logs install update-stack-yaml wait-for

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean setup build test run

clean:
	stack clean

update-stack-yaml:
	@if [ "`uname`" = "Darwin" ]; then \
	  sed -i '' 's/container-name: ".*"/container-name: "$(PROJECT_NAME)"/g' stack.yaml; \
	  sed -i '' 's/ARG PROJECT_NAME=.*/ARG PROJECT_NAME=$(PROJECT_NAME)/g' Dockerfile; \
	else \
	  sed -i 's/container-name: ".*"/container-name: "$(PROJECT_NAME)"/g' stack.yaml; \
	  sed -i 's/ARG PROJECT_NAME=.*/ARG PROJECT_NAME=$(PROJECT_NAME)/g' Dockerfile; \
	fi

setup: update-stack-yaml
	stack setup
	stack test --only-dependencies

format: setup
	stylish-haskell --config .stylish-haskell.yaml -r -i ./**/*.hs

build: setup
	stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"

test: setup docker-up wait-for
	stack test --fast

coverage: setup
	stack test --coverage --fast --haddock

watch-test: setup
	stack test --fast --file-watch --watch-all

watch-coverage: setup
	stack test --coverage --fast --file-watch --watch-all --haddock

ghcid: setup
	ghcid --command "stack ghci test/APITests.hs"

run: setup
	# Run basic server
	time stack exec run-server

# Run server variants
run-cache: setup
	# Requires redis-server running
	stack exec run-server -- cache

run-esq: setup
	stack exec run-server -- esq

# Database migrations
migrate: setup
	stack exec migrate-db

migrate-esq: setup
	stack exec migrate-db -- esq

release: setup
	stack build -j4 --ghc-options "-j16 +RTS -A256m -RTS" --docker

docker-build: release
	# 멀티스테이지 Dockerfile 사용 (docker/Dockerfile)
	docker build --build-arg PROJECT_NAME=$(PROJECT_NAME) -t $(PROJECT_NAME):latest -f docker/Dockerfile .

docker-build-multi: 
	docker build --build-arg PROJECT_NAME=$(PROJECT_NAME) -t $(PROJECT_NAME):latest -f docker/Dockerfile .

docker-run: docker-build
	# 앱 포트 8000 노출 및 데이터 볼륨 마운트
	mkdir -p ./data
	docker run -it --rm \
	  --name $(PROJECT_NAME)-app \
	  -p 8000:8000 \
	  -v $(PWD)/data:/app/data \
	  $(PROJECT_NAME):latest

docker-run-multi: docker-build-multi
	docker run -it --rm -p 8000:8000 $(PROJECT_NAME):latest

# Docker services management (matches README documentation)
docker-up:
	docker compose -f docker/docker-compose.yaml up postgres redis -d

docker-up-all:
	docker compose -f docker/docker-compose.yaml up --build -d

docker-down:
	docker compose -f docker/docker-compose.yaml down --volumes

docker-logs:
	docker compose -f docker/docker-compose.yaml logs -f

docker-build-app:
	docker compose -f docker/docker-compose.yaml build app

docker-restart-app:
	docker compose -f docker/docker-compose.yaml restart app

# Legacy aliases for backward compatibility
docker-compose-up: docker-up-all

docker-compose-down: docker-down

docker-compose-logs: docker-logs

# Wait for services to be ready
wait-for:
	@echo "Waiting for services to be ready..."
	@timeout 15 bash -c 'until docker compose -f docker/docker-compose.yaml exec postgres pg_isready -U postgres; do sleep 1; done' || true
	@timeout 15 bash -c 'until docker compose -f docker/docker-compose.yaml exec redis redis-cli ping; do sleep 1; done' || true

install: release
	mkdir -p $(HOME)/.local/bin
	cp $(shell stack path --local-install-root)/bin/$(PROJECT_NAME)-exe $(HOME)/.local/bin/
