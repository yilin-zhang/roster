#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

echo "[1/7] Check parentheses"
emacs --batch -Q -l scripts/roster-check.el -f roster-check-parens

echo "[2/7] Check indentation"
./scripts/check-indent.sh

echo "[3/7] Byte compile"
emacs --batch -L . -f batch-byte-compile \
  roster-core.el roster-opencode.el roster-claude.el \
  roster-codex.el roster-pi.el roster.el

echo "[4/7] Remove generated .elc"
find . -name '*.elc' -delete

echo "[5/7] Checkdoc"
./scripts/check-checkdoc.sh

echo "[6/7] Check Python bridge"
./scripts/check-python.sh

echo "[7/7] Package lint + tests"
./scripts/check-package-lint.sh
./scripts/check-ert.sh

echo "All checks passed."
