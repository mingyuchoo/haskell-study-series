.PHONY: build test run demo install lint format eval
build:
	stack build

test:
	stack test

run:
	stack run my-org-exe

demo:
	env -u MY_ORG_TEST_DATABASE_URL MY_ORG_DEMO=1 stack run my-org-exe

install: build

lint:
	stack build --ghc-options=-Werror

format:
	@echo 'Use the existing Haskell style; no formatter dependency is installed.'

eval: test
