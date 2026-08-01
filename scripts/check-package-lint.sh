#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PACKAGE_LINT_GIT_URL="https://github.com/purcell/package-lint.git"
PACKAGE_LINT_COMMIT="1c37329703a507fa357302cf6fc29d4f2fe631a8"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT
PACKAGE_LINT_DIR="$TMP_DIR/package-lint"
LOCAL_PACKAGE_LINT_DIR="$REPO_ROOT/package-lint"

if command -v git >/dev/null 2>&1; then
  git init "$PACKAGE_LINT_DIR" >/dev/null 2>&1 || true
  git -C "$PACKAGE_LINT_DIR" remote add origin "$PACKAGE_LINT_GIT_URL" >/dev/null 2>&1 || true
  git -C "$PACKAGE_LINT_DIR" fetch --depth 1 origin "$PACKAGE_LINT_COMMIT" >/dev/null 2>&1 || true
  git -C "$PACKAGE_LINT_DIR" checkout --detach FETCH_HEAD >/dev/null 2>&1 || true
fi

if [[ ! -f "$PACKAGE_LINT_DIR/package-lint.el" ]]; then
  if [[ -d "$LOCAL_PACKAGE_LINT_DIR" ]]; then
    PACKAGE_LINT_DIR="$LOCAL_PACKAGE_LINT_DIR"
  else
    echo "Error: failed to fetch package-lint source and no local fallback exists" >&2
    exit 1
  fi
fi

cd "$REPO_ROOT"
emacs --batch \
  -L . \
  -L "$PACKAGE_LINT_DIR" \
  -l "$PACKAGE_LINT_DIR/package-lint.el" \
  --eval "(progn
            (require 'package)
            (setq package-archives
                  '((\"gnu\" . \"https://elpa.gnu.org/packages/\")
                    (\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\")
                    (\"melpa\" . \"https://melpa.org/packages/\"))))" \
  -f package-lint-batch-and-exit \
  roster.el
