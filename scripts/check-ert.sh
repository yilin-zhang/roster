#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

emacs --batch -L . -L tests \
  -l roster \
  -l tests/roster-test.el \
  -f ert-run-tests-batch-and-exit
