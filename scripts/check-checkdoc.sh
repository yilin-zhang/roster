#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

emacs --batch --eval "(progn
  (require 'checkdoc)
	  (let ((files (append (directory-files \".\" nil \"\\\\.el\\\\'\")
	                       (directory-files \"tests\" t \"\\\\.el\\\\'\")))
        bad)
    (dolist (file files)
      (unless (checkdoc-file file)
        (push file bad)))
    (when bad
      (princ (format \"checkdoc failed: %S\\n\" bad))
      (kill-emacs 1))))"
