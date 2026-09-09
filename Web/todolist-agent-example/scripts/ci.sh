#!/usr/bin/env sh
set -eu
stack build --test --no-run-tests
stack test
