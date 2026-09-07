#!/bin/bash

set -euo pipefail

echo "Formatting Haskell files..."

# Resolve the project root so source directories and config files are found
# regardless of the caller's current directory.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
FOURMOLU_CONFIG="$PROJECT_ROOT/fourmolu.yaml"

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

# Format a single Haskell file.
format_file() {
  local file=$1
  echo "Processing $file"

  fourmolu --mode inplace --config "$FOURMOLU_CONFIG" "$file"
}

# Function to format files in a directory
format_directory() {
  local dir=$1

  # Check if directory exists
  if [ ! -d "$dir" ]; then
    echo "Directory $dir does not exist, skipping..."
    return
  fi

  # Check if there are any .hs files in the directory
  if [ -z "$(find "$dir" -name '*.hs' 2>/dev/null)" ]; then
    echo "No Haskell files found in $dir, skipping..."
    return
  fi

  # Process each .hs file in the directory
  find "$dir" -name "*.hs" | while read -r file; do
    if [ -f "$file" ]; then
      # Check for <| operator in test files
      if [[ "$dir" == "test" && $(grep -q "<|" "$file"; echo $?) -eq 0 ]]; then
        echo "Skipping $file (contains <| operator)"
      else
        format_file "$file"
      fi
    fi
  done
}

# Format files in common Haskell project directories
format_directory "src"
format_directory "app"
format_directory "test"

# Format Elm source files in the web application.  Exclude elm-stuff because it
# contains compiler-generated artifacts rather than project source.
format_elm_directory() {
  local dir=$1

  if [ ! -d "$dir" ]; then
    echo "Directory $dir does not exist, skipping Elm formatting..."
    return
  fi

  while IFS= read -r -d '' file; do
    echo "Processing $file"
    elm-format --yes "$file"
  done < <(find "$dir" \( -path "$dir/elm-stuff" -o -path "$dir/node_modules" \) -prune -o -type f -name '*.elm' -print0)
}

format_elm_directory "web"

# Format any other .hs files in the current directory
if [ -n "$(find . -maxdepth 1 -name '*.hs' 2>/dev/null)" ]; then
  echo "Processing Haskell files in current directory"
  find . -maxdepth 1 -name "*.hs" | while read -r file; do
    format_file "$file"
  done
fi

echo "Formatting complete"
