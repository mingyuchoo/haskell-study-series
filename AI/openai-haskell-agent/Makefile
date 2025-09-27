.PHONY: test coverage install update-stack-yaml

# Extract project name from directory path
PROJECT_NAME := $(shell basename $(CURDIR))

all: clean setup build test run

clean:
	stack clean

update-stack-yaml:
	sed -i '' 's/container-name: ".*"/container-name: "$(PROJECT_NAME)"/g' stack.yaml
	sed -i '' 's/ARG PROJECT_NAME=.*/ARG PROJECT_NAME=$(PROJECT_NAME)/g' Dockerfile

setup: update-stack-yaml
	stack --no-docker setup
	stack --no-docker test --only-dependencies

build: setup
	stack --no-docker build --fast -j4 --ghc-options "-j16 +RTS -A256m -RTS"

test: setup
	stack --no-docker test --fast

coverage: setup
	stack --no-docker test --coverage --fast --haddock

watch-test: setup
	stack --no-docker test --fast --file-watch --watch-all

watch-coverage: setup
	stack --no-docker test --coverage --fast --file-watch --watch-all --haddock

ghcid: setup
	ghcid --command "stack --no-docker ghci test/Spec.hs"

run: setup
	time stack --no-docker exec $(PROJECT_NAME)-exe

release: setup
	stack build --docker

docker-build: release
	docker build --build-arg PROJECT_NAME=$(PROJECT_NAME) -t $(PROJECT_NAME):latest -f Dockerfile .

docker-run: docker-build
	docker run -it --rm -p 80:8080 $(PROJECT_NAME):latest

install: release
	mkdir -p $(HOME)/.local/bin
	cp $(shell stack --no-docker path --local-install-root)/bin/$(PROJECT_NAME)-exe $(HOME)/.local/bin/
