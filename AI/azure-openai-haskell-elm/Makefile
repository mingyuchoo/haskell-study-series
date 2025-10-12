.PHONY: test coverage install update-stack-yaml frontend-build frontend-clean

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean setup frontend-build build test run

clean:
	cd backend && stack clean
	$(MAKE) frontend-clean

frontend-clean:
	cd frontend && rm -f public/elm.js
	rm -rf backend/static

frontend-build:
	cd frontend && elm make src/Main.elm --output=public/elm.js --optimize
	mkdir -p backend/static
	cp frontend/public/elm.js backend/static/
	cp frontend/public/index.html backend/static/

update-stack-yaml:
	sed -i 's/container-name: ".*"/container-name: "$(PROJECT_NAME)"/g' backend/stack.yaml
	# 루트 Dockerfile과 멀티스테이지 docker/Dockerfile 모두 PROJECT_NAME 갱신
	- sed -i 's/ARG PROJECT_NAME=.*/ARG PROJECT_NAME=$(PROJECT_NAME)/g' Dockerfile
	- sed -i 's/ARG PROJECT_NAME=.*/ARG PROJECT_NAME=$(PROJECT_NAME)/g' docker/Dockerfile

setup: update-stack-yaml
	cd backend && stack setup
	cd backend && stack test --only-dependencies

format: setup
	cd backend && stylish-haskell --config .stylish-haskell.yaml -r -i ./**/*.hs

build: setup frontend-build
	cd backend && stack build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"

test: setup
	cd backend && stack test --fast

coverage: setup
	cd backend && stack test --coverage --fast --haddock

watch-test: setup
	cd backend && stack test --fast --file-watch --watch-all

watch-coverage: setup
	cd backend && stack test --coverage --fast --file-watch --watch-all --haddock

ghcid: setup
	cd backend && ghcid --command "stack ghci test/Spec.hs"

run: setup
	cd backend && time stack exec backend-exe

release: setup frontend-build
	cd backend && stack build -j4 --ghc-options "-j16 +RTS -A256m -RTS" --docker

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

docker-compose-up:
	docker compose -f docker/docker-compose.yaml --env-file backend/.env up --build -d

docker-compose-down:
	docker compose -f docker/docker-compose.yaml --env-file backend/.env down --volumes

docker-compose-logs:
	docker compose -f docker/docker-compose.yaml --env-file backend/.env logs -f

install: release
	mkdir -p $(HOME)/.local/bin
	cp $(shell cd backend && stack path --local-install-root)/bin/backend-exe $(HOME)/.local/bin/
