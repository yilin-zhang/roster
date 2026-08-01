#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

if [[ "${1:-}" == "--fix" ]]; then
  export ROSTER_FIX_INDENT=1
fi

# Load test definitions so their custom macro indentation declarations apply.
emacs --batch -Q -L . -L tests \
  -l roster -l tests/roster-test.el -l scripts/roster-check.el \
  -f roster-check-indent
