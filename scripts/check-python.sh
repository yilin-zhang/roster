#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PYTHON_CACHE_DIR="$(mktemp -d)"
trap 'rm -rf "$PYTHON_CACHE_DIR"' EXIT

cd "$REPO_ROOT"
PYTHONPYCACHEPREFIX="$PYTHON_CACHE_DIR" \
  python3 -m py_compile scripts/roster-claude-sdk.py
