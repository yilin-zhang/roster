#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

echo "[1/6] Check parentheses"
emacs --batch --eval '(dolist (file (append (directory-files "." t "\\.el\\'")
                                            (directory-files "tests" t "\\.el\\'")))
                        (with-temp-buffer
                          (insert-file-contents file)
                          (check-parens)))'

echo "[2/6] Check indentation"
./scripts/check-indent.sh

echo "[3/6] Byte compile"
emacs --batch -L . -f batch-byte-compile \
  roster-core.el roster-opencode.el roster-claude.el \
  roster-codex.el roster-pi.el roster.el

echo "[4/6] Remove generated .elc"
find . -name '*.elc' -delete

echo "[5/6] Checkdoc"
./scripts/check-checkdoc.sh

echo "[6/6] Package lint + tests"
./scripts/check-package-lint.sh
./scripts/check-ert.sh

echo "All checks passed."
