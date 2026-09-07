#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

require_command() {
  local command_name=$1

  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Required command not found: $command_name" >&2
    exit 1
  fi
}

require_command fourmolu
require_command elm-format
require_command elm
require_command stack
require_command npm
require_command npx
require_command lsof

stop_existing_server() {
  local port=$1
  local process_id
  local process_executable
  local listener_output
  local attempts=0
  local process_ids=()
  local live_process_ids=()

  if listener_output="$(lsof -t -nP -iTCP:"$port" -sTCP:LISTEN 2>&1)"; then
    if [ -n "$listener_output" ]; then
      mapfile -t process_ids <<<"$listener_output"
    fi
  elif [ -z "$listener_output" ]; then
    return
  else
    echo "Could not inspect listeners on port $port:" >&2
    echo "$listener_output" >&2
    exit 1
  fi

  for process_id in "${process_ids[@]}"; do
    if [ ! -e "/proc/$process_id/exe" ]; then
      continue
    fi

    process_executable="$(basename "$(readlink -f "/proc/$process_id/exe")")"

    if [ "$process_executable" != "GeneralService-exe" ]; then
      echo "Port $port is in use by a non-GeneralService process (PID $process_id): $process_executable" >&2
      echo "Refusing to stop it automatically." >&2
      exit 1
    fi

    live_process_ids+=("$process_id")
  done

  if [ "${#live_process_ids[@]}" -eq 0 ]; then
    return
  fi

  echo "Stopping existing GeneralService server on port $port..."
  kill "${live_process_ids[@]}"

  while lsof -t -nP -iTCP:"$port" -sTCP:LISTEN >/dev/null 2>&1; do
    if [ "$attempts" -ge 50 ]; then
      echo "Existing GeneralService server did not stop on port $port." >&2
      exit 1
    fi

    sleep 0.1
    attempts=$((attempts + 1))
  done
}

echo "Formatting Haskell and Elm source files..."
"$SCRIPT_DIR/format.sh"

echo "Building Elm frontend..."
(
  cd web
  elm make src/Main.elm --output=elm.js
)

echo "Building Haskell backend..."
stack build

echo "Running Elm frontend tests..."
(
  cd web

  # node_modules is git-ignored, so a fresh checkout has no elm-test binary.
  # Install the pinned dev dependencies before running the tests.
  if [ ! -x node_modules/.bin/elm-test ]; then
    echo "Installing Elm test dependencies..."
    if [ -f package-lock.json ]; then
      npm ci
    else
      npm install
    fi
  fi

  npx --no-install elm-test
)

echo "Running Haskell backend tests..."
stack test

SERVER_PORT="${PORT:-3000}"
stop_existing_server "$SERVER_PORT"

echo "Starting GeneralService at http://localhost:${SERVER_PORT}"
exec stack run
