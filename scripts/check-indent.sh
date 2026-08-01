#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

emacs --batch --eval "(progn
  (require 'cl-lib)
  (let ((files (append (directory-files \".\" nil \"\\\\.el\\\\'\")
                       (mapcar (lambda (file) (concat \"tests/\" file))
                               (directory-files \"tests\" nil \"\\\\.el\\\\'\"))))
        (bad-files '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((original (buffer-string)))
          (emacs-lisp-mode)
          (indent-region (point-min) (point-max))
          (unless (string-equal original (buffer-string))
            (push file bad-files)))))
    (if bad-files
        (progn
          (princ \"Indentation check failed for:\n\")
          (dolist (file (nreverse bad-files))
            (princ (format \"  %s\n\" file)))
          (kill-emacs 1))
      (princ \"Indentation looks good.\n\"))))"
