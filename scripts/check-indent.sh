#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

if [[ "${1:-}" == "--fix" ]]; then
  export ROSTER_FIX_INDENT=1
fi

# Macro indentation declarations are read straight out of the sources by
# `roster-check--register-indent-specs', so nothing needs loading here.
emacs --batch -Q -l scripts/roster-check.el -f roster-check-indent
