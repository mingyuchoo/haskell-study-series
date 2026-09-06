.DEFAULT_GOAL := build
.PHONY: build frontend frontend-test test run demo install lint format eval
frontend:
	npm ci
	npm run build

build: format
	$(MAKE) frontend
	stack build

frontend-test:
	npm ci
	npm test
	npm run check

test: frontend-test
	stack test

run:
	stack run my-org-exe

demo:
	env -u MY_ORG_TEST_DATABASE_URL MY_ORG_DEMO=1 stack run my-org-exe

install: build

lint:
	stack build --ghc-options=-Werror

format:
	bash scripts/format.sh

eval: test
