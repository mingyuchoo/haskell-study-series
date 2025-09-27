.PHONY: test coverage install update-stack-yaml run migrate migrate-esq run-cache run-esq

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean setup build test run

clean:
	stack clean

update-stack-yaml:
	sed -i 's/container-name: ".*"/container-name: "$(PROJECT_NAME)"/g' stack.yaml
	sed -i 's/ARG PROJECT_NAME=.*/ARG PROJECT_NAME=$(PROJECT_NAME)/g' Dockerfile

setup: update-stack-yaml
	stack setup
	stack test --only-dependencies

format: setup
	stylish-haskell --config .stylish-haskell.yaml -r -i ./**/*.hs

build: setup
	stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"

## Ensure local services are running for tests
.PHONY: docker-up docker-down wait-for

docker-up:
	# Start Postgres and Redis in the background if not already running
	docker compose up -d postgres redis

docker-down:
	# Stop services
	docker compose down

wait-for:
	@echo "Waiting for Postgres (5432) and Redis (6379) to be ready..."
	@i=0; \
	until nc -z 127.0.0.1 5432 && nc -z 127.0.0.1 6379; do \
	  i=$$((i+1)); \
	  if [ $$i -ge 60 ]; then echo "Timeout waiting for services"; exit 1; fi; \
	  sleep 1; \
	done; \
	echo "Services are ready."

test: setup docker-up wait-for
	stack test --fast

coverage: setup
	stack test --coverage --fast --haddock

watch-test: setup
	stack test --fast --file-watch --watch-all

watch-coverage: setup
	stack test --coverage --fast --file-watch --watch-all --haddock

ghcid: setup
	ghcid --command "stack ghci test/Spec.hs"

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
	docker build --build-arg PROJECT_NAME=$(PROJECT_NAME) -t $(PROJECT_NAME):latest -f Dockerfile .

docker-run: docker-build
	docker run -it --rm -p 8000:8000 $(PROJECT_NAME):latest

install: release
	mkdir -p $(HOME)/.local/bin
	cp $(shell stack path --local-install-root)/bin/$(PROJECT_NAME)-exe $(HOME)/.local/bin/
