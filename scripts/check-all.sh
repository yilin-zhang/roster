#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

echo "[1/7] Check parentheses"
emacs --batch -Q -l scripts/roster-check.el -f roster-check-parens

echo "[2/7] Check indentation"
./scripts/check-indent.sh

echo "[3/7] Byte compile"
# `byte-compile-error-on-warn' makes warnings fail the build; without it
# `batch-byte-compile' exits 0 and real defects (a macro used before its
# definition, a misdeclared optional dependency) scroll past a green run.
# Glob rather than list files: a hand-maintained list silently stops
# covering a source file the moment a new one is added.
emacs --batch -L . \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile roster*.el

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
