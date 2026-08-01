;;; roster-check.el --- CI checks for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers invoked by the shell scripts in this directory.

;;; Code:

(require 'checkdoc)

(defconst roster-check--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the roster repository root.")

(defun roster-check--elisp-files ()
  "Return absolute paths of roster source and test Elisp files."
  (append (directory-files roster-check--root t "\\.el\\'")
          (directory-files (expand-file-name "tests" roster-check--root)
                           t "\\.el\\'")
          (directory-files (expand-file-name "scripts" roster-check--root)
                           t "\\.el\\'")))

(defun roster-check-parens ()
  "Check balanced parentheses in every roster Elisp file."
  (dolist (file (roster-check--elisp-files))
    (with-temp-buffer
      (insert-file-contents file)
      (check-parens))))

(defun roster-check-indent ()
  "Check or fix indentation in every roster Elisp file.
When ROSTER_FIX_INDENT is set, rewrite files instead of failing."
  (let (bad-files)
    (dolist (file (roster-check--elisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((original (buffer-string)))
          (emacs-lisp-mode)
          ;; Keep formatting independent of user and platform defaults.
          (setq-local indent-tabs-mode nil
                      tab-width 8)
          (untabify (point-min) (point-max))
          (indent-region (point-min) (point-max))
          (unless (string-equal original (buffer-string))
            (if (getenv "ROSTER_FIX_INDENT")
                (write-region (point-min) (point-max) file nil 'silent)
              (push file bad-files))))))
    (if bad-files
        (progn
          (princ "Indentation check failed for:\n")
          (dolist (file (nreverse bad-files))
            (princ (format "  %s\n" (file-relative-name file roster-check--root))))
          (kill-emacs 1))
      (princ "Indentation looks good.\n"))))

(defun roster-check-checkdoc ()
  "Run Checkdoc against every roster Elisp file."
  (let (bad-files)
    (dolist (file (roster-check--elisp-files))
      (unless (checkdoc-file file)
        (push file bad-files)))
    (when bad-files
      (princ (format "Checkdoc failed: %S\n" bad-files))
      (kill-emacs 1))))

(provide 'roster-check)

;;; roster-check.el ends here
