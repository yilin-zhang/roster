;;; roster-test-helpers.el --- Shared test helpers for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'roster)

;;; Helpers

(defmacro roster-test--with-sqlite-rows (rows &rest body)
  "Eval BODY with `roster--opencode-sqlite-rows' mocked to return ROWS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'roster--opencode-sqlite-rows) (lambda (_sql) ,rows)))
	 ,@body))

(provide 'roster-test-helpers)

;;; roster-test-helpers.el ends here
