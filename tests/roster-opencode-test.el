;;; roster-opencode-test.el --- OpenCode tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-test-helpers)

;;; Session loading — OpenCode backend

(ert-deftest roster-load-sessions-only-returns-root-sessions ()
  (let ((roster-enabled-tools '(opencode)))
    (roster-test--with-sqlite-rows
        '(("ses_root"     "Root Session"     "/tmp/root"     "proj_root"     "1700000000000" "")
          ("ses_archived" "Archived Session" "/tmp/archived" "proj_archived" "1700000001000" "1700000002000"))
      (let ((sessions (roster--load-sessions)))
        (should (= (length sessions) 2))
        (should (equal (mapcar (lambda (s) (plist-get s :id)) sessions)
                       '("ses_archived" "ses_root")))
        (should (= (plist-get (car sessions) :time-archived) 1700000002000))
        (should-not (plist-get (cadr sessions) :time-archived))))))

(ert-deftest roster-load-sessions-handles-pipe-in-directory ()
  "Pipe characters in directory paths must not corrupt adjacent fields."
  (let ((roster-enabled-tools '(opencode)))
    (roster-test--with-sqlite-rows
        '(("ses_pipe" "Pipe Dir" "/tmp/a|b" "proj_pipe" "1700000000000" ""))
      (let* ((sessions (roster--load-sessions))
             (session (car sessions)))
        (should (= (length sessions) 1))
        (should (equal (plist-get session :id) "ses_pipe"))
        (should (equal (plist-get session :title) "Pipe Dir"))
        (should (string-suffix-p "/tmp/a|b" (plist-get session :directory)))
        (should (equal (plist-get session :project-id) "proj_pipe"))
        (should (= (plist-get session :time-updated) 1700000000000))
        (should-not (plist-get session :time-archived))))))

;;; Project resolution

(ert-deftest roster-resolve-target-project-prefers-exact-match ()
  (cl-letf (((symbol-function 'roster--opencode-project-for-directory)
             (lambda (_directory)
               '(:id "proj_exact" :worktree "/tmp/site-lisp/decklet" :name "decklet")))
            ((symbol-function 'roster--opencode-projects-containing-directory)
             (lambda (_directory)
               (error "should not inspect parents when exact match exists")))
            ((symbol-function 'roster--opencode-global-project)
             (lambda ()
               (error "should not fall back to global when exact match exists"))))
	(should (equal (plist-get (roster--opencode-resolve-target-project "/tmp/site-lisp/decklet") :id)
			       "proj_exact"))))

(ert-deftest roster-resolve-target-project-falls-back-to-global ()
  (cl-letf (((symbol-function 'roster--opencode-project-for-directory)
             (lambda (_directory) nil))
            ((symbol-function 'roster--opencode-projects-containing-directory)
             (lambda (_directory) nil))
            ((symbol-function 'roster--opencode-global-project)
             (lambda ()
               '(:id "global" :worktree "/"))))
	(should (equal (plist-get (roster--opencode-resolve-target-project "/tmp/site-lisp") :id)
			       "global"))))

(ert-deftest roster-resolve-target-project-uses-only-parent-project ()
  (cl-letf (((symbol-function 'roster--opencode-project-for-directory)
             (lambda (_directory) nil))
            ((symbol-function 'roster--opencode-projects-containing-directory)
             (lambda (_directory)
               '((:id "proj_parent" :worktree "/tmp/root" :name "root"))))
            ((symbol-function 'roster--opencode-global-project)
             (lambda ()
               (error "should not fall back to global when parent exists"))))
	(should (equal (plist-get (roster--opencode-resolve-target-project "/tmp/root/subdir") :id)
			       "proj_parent"))))

;;; SQLite integration (requires sqlite3 CLI or Emacs 29+ built-in)

(ert-deftest roster-sqlite-rows-returns-correct-fields ()
  "Verify roster--opencode-sqlite-rows returns correct fields from a real temp database.
Includes a pipe character in a field value to confirm it is not misinterpreted."
  (let* ((db (make-temp-file "roster-test-" nil ".db"))
         (roster-opencode-db-path db)
         (roster-enabled-tools '(opencode))
         (conn (sqlite-open db)))
    (unwind-protect
        (progn
          (sqlite-execute conn
			              (concat "CREATE TABLE session "
				                  "(id TEXT, title TEXT, directory TEXT, "
				                  "project_id TEXT, time_updated INTEGER, "
				                  "time_archived INTEGER, parent_id TEXT)"))
          (sqlite-execute conn
			              (concat "INSERT INTO session VALUES "
				                  "('ses_int','Int|Test','/tmp/a|b','proj_1',1700000000000,NULL,NULL)"))
          (sqlite-close conn)
          (setq conn nil)
          (let ((sessions (roster--load-sessions)))
            (should (= (length sessions) 1))
            (let ((s (car sessions)))
              (should (equal (plist-get s :id) "ses_int"))
              (should (equal (plist-get s :title) "Int|Test"))
              (should (string-suffix-p "/tmp/a|b" (plist-get s :directory)))
              (should (equal (plist-get s :project-id) "proj_1"))
              (should (= (plist-get s :time-updated) 1700000000000))
              (should-not (plist-get s :time-archived)))))
      (when conn (sqlite-close conn))
      (delete-file db))))

(provide 'roster-opencode-test)

;;; roster-opencode-test.el ends here
