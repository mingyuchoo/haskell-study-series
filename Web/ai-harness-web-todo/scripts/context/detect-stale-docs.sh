#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

./scripts/context/generate-db-schema.sh --check
./scripts/context/generate-api-index.sh --check
./scripts/context/generate-dependency-map.sh --check
./scripts/context/generate-route-map.sh --check

max_age_days="${CONTEXT_MAX_PLAN_AGE_DAYS:-30}"

python3 - "$repo_root" "$max_age_days" <<'PY'
from __future__ import annotations

import sys
import time
from pathlib import Path

root = Path(sys.argv[1])
max_age_days = int(sys.argv[2])
cutoff = time.time() - max_age_days * 24 * 60 * 60
stale: list[str] = []

for plan in (root / "docs/plans/active").glob("*.md"):
    if plan.name == "README.md":
        continue
    if plan.stat().st_mtime < cutoff:
        stale.append(str(plan.relative_to(root)))

if stale:
    print(f"{max_age_days}일보다 오래된 활성 계획을 발견했습니다:", file=sys.stderr)
    for path in stale:
        print(f"- {path}", file=sys.stderr)
    raise SystemExit(1)

print("활성 계획 수명 검사 통과")
PY
