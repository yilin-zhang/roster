;;; roster-core-test.el --- Core tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for shared roster helpers.

;;; Code:

(require 'roster-test-helpers)

(ert-deftest roster-read-json-preserves-data-shape-across-parsers ()
  "Native and compatibility parsers must return the same roster data shape."
  (let ((input (concat
                "{\"items\":[{\"type\":\"text\",\"text\":\"中文\","
                "\"falseValue\":false,\"nullValue\":null}]}"))
        (expected '(:items ((:type "text" :text "中文"
                                   :falseValue nil :nullValue nil)))))
    (dolist (native '(t nil))
      (cl-letf (((symbol-function 'json-available-p)
                 (lambda () native)))
        (should (equal (roster--read-json input) expected))))))

(ert-deftest roster-read-json-returns-nil-for-malformed-input ()
  (should-not (roster--read-json "{")))

(provide 'roster-core-test)

;;; roster-core-test.el ends here
