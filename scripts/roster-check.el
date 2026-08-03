;;; roster-check.el --- CI checks for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers invoked by the shell scripts in this directory.

;;; Code:

(require 'checkdoc)

;; Load the libraries whose macros appear in the sources, so their own
;; `declare' specs are registered.  Without `cl-lib', for instance,
;; `cl-letf' bodies get measured against the fallback rule.
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

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

(defun roster-check--register-indent-declaration (form)
  "Register the `lisp-indent-function' spec FORM declares, if it declares one."
  (when (and (proper-list-p form)
             (memq (car form) '(defmacro cl-defmacro))
             (symbolp (nth 1 form)))
    (let ((declaration (seq-find (lambda (subform)
                                   (and (consp subform)
                                        (eq (car subform) 'declare)))
                                 (nthcdr 3 form))))
      (when-let* ((spec (assq 'indent (cdr declaration))))
        (put (nth 1 form) 'lisp-indent-function (cadr spec))))))

(defun roster-check--register-indent-form (form)
  "Register `lisp-indent-function' specs declared anywhere within FORM.
Walks FORM recursively so macros wrapped in conditionals are found too.
The spine is walked iteratively and dotted pairs are tolerated, so
quoted test data cannot abort the scan."
  (when (consp form)
    (roster-check--register-indent-declaration form)
    (while (consp form)
      (roster-check--register-indent-form (car form))
      (setq form (cdr form)))))

(defun roster-check--register-indent-specs (files)
  "Register indentation specs declared by macros defined in FILES.
The indentation check never evaluates the sources, so `declare' forms
inside `defmacro' would otherwise be invisible and macro call sites
would be measured against Emacs' fallback rule instead of the rule the
macro actually declares."
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (roster-check--register-indent-form (read (current-buffer))))
        (end-of-file nil)
        ;; A file we cannot fully read still contributes whatever it
        ;; declared before the unreadable form; the indentation check
        ;; itself will report the real problem.
        (error nil)))))

(defun roster-check-indent ()
  "Check or fix indentation in every roster Elisp file.
When ROSTER_FIX_INDENT is set, rewrite files instead of failing."
  (roster-check--register-indent-specs (roster-check--elisp-files))
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
