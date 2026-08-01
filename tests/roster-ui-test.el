;;; roster-ui-test.el --- UI and dispatch tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-test-helpers)

;;; List UI

(ert-deftest roster-project-scoped-sessions-filters-by-project-root ()
  (let ((sessions '((:id "a" :directory "/repo/project")
                    (:id "b" :directory "/repo/project/subdir")
                    (:id "c" :directory "/repo/other"))))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) 'fake-project))
              ((symbol-function 'project-root)
               (lambda (_project) "/repo/project/")))
      (let ((default-directory "/repo/project/subdir/"))
        (should (equal (mapcar (lambda (session) (plist-get session :id))
			       (roster--project-scoped-sessions sessions))
		       '("a" "b")))))))

(ert-deftest roster-refresh-hides-archived-when-disabled ()
  (with-temp-buffer
    (let ((roster-source-function
           (lambda ()
             '((:id "active" :title "Active" :directory "/tmp/a" :time-updated 1700000000000)
               (:id "archived" :title "Archived" :directory "/tmp/b" :time-updated 1700000001000 :time-archived 1700000002000))))
          (roster-show-archived nil))
      (roster-mode)
      (roster--populate)
      (should (= (length tabulated-list-entries) 1))
      (should (equal (caar tabulated-list-entries) '(opencode . "active"))))))

(ert-deftest roster-entry-includes-derived-columns ()
  (let* ((session '(:id "ses_1"
			:title "Decklet Dev"
			:directory "/tmp/decklet"
			:time-updated 1700000000000
			:time-archived 1700000005000))
         (entry (roster--entry session))
         (columns (cadr entry)))
    (should (equal (car entry) '(opencode . "ses_1")))
    (should (string-match-p "Decklet Dev" (aref columns 0)))
    (should (equal (aref columns 1) "OC"))        ; tool tag (no :tool → opencode)
    (should (equal (aref columns 2) "ARCHIVED"))  ; state
    (should (equal (aref columns 3) "decklet"))   ; project
    (should (equal (aref columns 4) "/tmp/decklet")) ; directory
    (should (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" (aref columns 5)))))

(ert-deftest roster-entry-shows-claude-tag ()
  (let* ((session '(:id "cc_1"
			:title "My Claude Session"
			:directory "/tmp/proj"
			:time-updated 1700000000000
			:tool claude))
         (columns (cadr (roster--entry session))))
    (should (equal (aref columns 1) "CC"))
    (should (equal (aref columns 2) "ACTIVE"))))

(ert-deftest roster-entry-shows-codex-tag ()
  (let* ((session '(:id "cx_1"
			:title "My Codex Session"
			:directory "/tmp/proj"
			:time-updated 1700000000000
			:tool codex))
         (columns (cadr (roster--entry session))))
    (should (equal (aref columns 1) "CX"))
    (should (equal (aref columns 2) "ACTIVE"))))

(ert-deftest roster-entry-shows-pi-tag ()
  (let* ((session '(:id "pi_1"
			:title "My pi Session"
			:directory "/tmp/proj"
			:time-updated 1700000000000
			:tool pi))
         (columns (cadr (roster--entry session))))
    (should (equal (aref columns 1) "PI"))
    (should (equal (aref columns 2) "ACTIVE"))))

;;; Tool dispatch

(ert-deftest roster-rename-session-dispatches-by-tool ()
  (let (captured)
    (cl-letf (((symbol-function 'roster--claude-rename-session-command)
               (lambda (_s) (setq captured 'claude) t))
              ((symbol-function 'roster--codex-rename-session-command)
               (lambda (_s) (setq captured 'codex) t))
              ((symbol-function 'roster--pi-rename-session-command)
               (lambda (_s) (setq captured 'pi) t))
              ((symbol-function 'roster--opencode-rename-session-command)
               (lambda (_s) (setq captured 'opencode) t)))
      (roster--rename-session-command '(:id "c1" :tool claude))
      (should (eq captured 'claude))
      (roster--rename-session-command '(:id "cx1" :tool codex))
      (should (eq captured 'codex))
      (roster--rename-session-command '(:id "pi1" :tool pi))
      (should (eq captured 'pi))
      (roster--rename-session-command '(:id "o1"))
      (should (eq captured 'opencode)))))

(ert-deftest roster-set-archived-command-dispatches-by-tool ()
  (let (captured)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'roster--do-archive-session)
               (lambda (session _archived)
                 (setq captured (or (plist-get session :tool) 'opencode)))))
      (roster--set-archived-command '(:id "c1" :title "T" :tool claude) t)
      (should (eq captured 'claude))
      (roster--set-archived-command '(:id "cx1" :title "T" :tool codex) t)
      (should (eq captured 'codex))
      (roster--set-archived-command '(:id "pi1" :title "T" :tool pi) t)
      (should (eq captured 'pi))
      (roster--set-archived-command '(:id "o1" :title "T") nil)
      (should (eq captured 'opencode)))))

(ert-deftest roster-delete-session-dispatches-by-tool ()
  (let (captured)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
              ((symbol-function 'roster--claude-delete-session)
               (lambda (_s) (setq captured 'claude)))
              ((symbol-function 'roster--codex-delete-session)
               (lambda (_s) (setq captured 'codex)))
              ((symbol-function 'roster--pi-delete-session)
               (lambda (_s) (setq captured 'pi)))
              ((symbol-function 'roster--opencode-delete-session)
               (lambda (_s) (setq captured 'opencode))))
      (roster--delete-session-command '(:id "c1" :title "T" :tool claude :directory "/d"))
      (should (eq captured 'claude))
      (roster--delete-session-command '(:id "cx1" :title "T" :tool codex :directory "/d"))
      (should (eq captured 'codex))
      (roster--delete-session-command '(:id "pi1" :title "T" :tool pi :directory "/d"))
      (should (eq captured 'pi))
      (roster--delete-session-command '(:id "o1" :title "T" :directory "/d"))
      (should (eq captured 'opencode)))))

;;; Mark system

(defmacro roster-test--with-list-buffer (sessions &rest body)
  "Eval BODY in a temporary `roster-mode' buffer showing SESSIONS.
Point starts at the first data row (header is in header-line-format,
not a buffer line)."
  (declare (indent 1))
  `(with-temp-buffer
     (roster-mode)
     (setq-local roster-source-function (lambda () ,sessions))
     (setq-local roster-show-archived t)
     (roster--populate)
     (tabulated-list-print t)
     (goto-char (point-min))
     ,@body))

(ert-deftest roster-session-at-point-distinguishes-tools-with-the-same-id ()
  (roster-test--with-list-buffer
      '((:id "same" :title "Claude" :directory "/c" :time-updated 2000 :tool claude)
	(:id "same" :title "Codex" :directory "/x" :time-updated 1000 :tool codex))
    (should (eq (plist-get (roster--session-at-point) :tool) 'claude))
    (forward-line 1)
    (should (eq (plist-get (roster--session-at-point) :tool) 'codex))))

(ert-deftest roster-mode-keeps-marks-buffer-local ()
  (let ((first (generate-new-buffer " *roster-first*"))
        (second (generate-new-buffer " *roster-second*")))
    (unwind-protect
        (progn
          (with-current-buffer first
            (roster-mode)
            (puthash '(codex . "one") t roster--marked))
          (with-current-buffer second
            (roster-mode)
            (should (zerop (hash-table-count roster--marked))))
          (should-not (eq (buffer-local-value 'roster--marked first)
                          (buffer-local-value 'roster--marked second))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest roster--marked-keys-returns-marked ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000)
	(:id "s2" :title "B" :directory "/b" :time-updated 900))
    (puthash '(opencode . "s1") t roster--marked)
    (puthash '(opencode . "s2") t roster--marked)
    (should (equal (sort (roster--marked-keys)
                         (lambda (a b) (string< (cdr a) (cdr b))))
                   '((opencode . "s1") (opencode . "s2"))))))

(ert-deftest roster-mark-toggles-on-and-off ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000))
    ;; First m at point-min: marks the session and advances past it.
    (roster-mark)
    (should (gethash '(opencode . "s1") roster--marked))
    ;; Return to the session row and m again: should unmark.
    (goto-char (point-min))
    (roster-mark)
    (should-not (gethash '(opencode . "s1") roster--marked))))

(ert-deftest roster-unmark-removes-mark ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000))
    (puthash '(opencode . "s1") t roster--marked)
    (roster-unmark)
    (should-not (gethash '(opencode . "s1") roster--marked))))

(ert-deftest roster-unmark-all-clears-all ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000)
	(:id "s2" :title "B" :directory "/b" :time-updated 900))
    (puthash '(opencode . "s1") t roster--marked)
    (puthash '(opencode . "s2") t roster--marked)
    (roster-unmark-all)
    (should (zerop (hash-table-count roster--marked)))))

(ert-deftest roster--nearest-surviving-session-prefers-forward ()
  "When two candidates are equidistant, prefer the one on a following line."
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 3000)
	(:id "s2" :title "B" :directory "/b" :time-updated 2000)
	(:id "s3" :title "C" :directory "/c" :time-updated 1000))
    ;; Move to the middle row: sessions are sorted newest-first, so
    ;; s1 is line 1, s2 is line 2, s3 is line 3.
    (forward-line 1)
    (let ((nearest (roster--nearest-surviving-session
                    '((opencode . "s2")))))
      ;; s1 and s3 are equidistant; s3 follows so it should be preferred
      (should (equal nearest '(opencode . "s3"))))))

(ert-deftest roster-delete-calls-do-delete ()
  (let (deleted)
    (roster-test--with-list-buffer
	'((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool claude
               :encoded-dir "-a")
	  (:id "s2" :title "B" :directory "/b" :time-updated 900 :tool claude
               :encoded-dir "-b"))
      (puthash '(claude . "s1") t roster--marked)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
		((symbol-function 'roster--do-delete-session)
                 (lambda (s) (push (plist-get s :id) deleted)))
		((symbol-function 'revert-buffer) #'ignore)
		((symbol-function 'recenter) #'ignore))
	(roster-delete)
	(should (equal deleted '("s1")))
	(should (zerop (hash-table-count roster--marked)))))))

(ert-deftest roster-archive-calls-do-archive ()
  (let (archived-calls)
    (roster-test--with-list-buffer
	'((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool claude
               :encoded-dir "-a"))
      (puthash '(claude . "s1") t roster--marked)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
		((symbol-function 'roster--do-archive-session)
                 (lambda (s a) (push (cons (plist-get s :id) a) archived-calls)))
		((symbol-function 'revert-buffer) #'ignore))
	(roster-archive)
	;; s1 is active (no :time-archived), so it should be archived (t)
	(should (equal archived-calls '(("s1" . t))))
	(should (zerop (hash-table-count roster--marked)))))))

;;; Utilities

(ert-deftest roster-run-command-falls-back-when-directory-missing ()
  (let (captured-default-directory)
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (_command _in buffer)
                 (setq captured-default-directory default-directory)
                 (with-current-buffer (if (eq buffer t) (current-buffer) buffer)
                   (insert "ok"))
                 0)))
      (should (equal (roster--run-command "/tmp/roster-does-not-exist" "true") "ok"))
      (should (equal captured-default-directory
		     (file-name-as-directory (expand-file-name "~")))))))

(provide 'roster-ui-test)

;;; roster-ui-test.el ends here
