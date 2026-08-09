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
             '((:id "active" :title "Active" :directory "/tmp/a" :tool opencode
                    :time-updated 1700000000000)
               (:id "archived" :title "Archived" :directory "/tmp/b" :tool opencode
                    :time-updated 1700000001000 :time-archived 1700000002000))))
          (roster-show-archived nil))
      (roster-mode)
      (roster--populate)
      (should (= (length tabulated-list-entries) 1))
      (should (equal (caar tabulated-list-entries) '(opencode . "active"))))))

(ert-deftest roster-toggle-archived-renders-loaded-snapshot-only ()
  (roster-test--with-list-buffer
      '((:id "active" :title "Active" :directory "/a" :tool opencode
             :time-updated 2000)
        (:id "archived" :title "Archived" :directory "/b" :tool opencode
             :time-updated 1000 :time-archived 1500))
    (setq-local roster-source-function
                (lambda () (error "archive toggle reloaded a backend")))
    (puthash '(opencode . "active") t roster--marked)
    (roster-toggle-archived)
    (should (equal (mapcar #'car tabulated-list-entries)
                   '((opencode . "active"))))
    (should (gethash '(opencode . "active") roster--mark-overlays))
    (roster-toggle-archived)
    (should (= (length tabulated-list-entries) 2))))

(ert-deftest roster-backend-source-loads-archives-only-when-needed ()
  (with-temp-buffer
    (roster-mode)
    (let (calls)
      (setq-local roster-backend-source-p t)
      (setq-local roster-show-archived nil)
      (setq-local roster-source-function
                  (lambda (include-archived)
                    (push include-archived calls)
                    (if include-archived
                        '((:id "active" :title "Active" :directory "/a"
                               :tool opencode :time-updated 2000)
                          (:id "archived" :title "Archived" :directory "/b"
                               :tool opencode :time-updated 1000
                               :time-archived 1500))
                      '((:id "active" :title "Active" :directory "/a"
                             :tool opencode :time-updated 2000)))))
      (roster--populate)
      (should-not roster--snapshot-includes-archived)
      (should (equal calls '(nil)))
      (tabulated-list-print t)
      (roster-toggle-archived)
      (should roster--snapshot-includes-archived)
      (should (equal calls '(t nil)))
      (should (= (length tabulated-list-entries) 2)))))

(ert-deftest roster-reload-tools-replaces-only-requested-backend ()
  (let (loaded)
    (unwind-protect
        (progn
          (roster-register-backend
           (roster-backend-create
            :id 'reload-a :label "A" :face 'default
            :load (lambda (_include-archived)
                    (push 'reload-a loaded)
                    '((:id "a-new" :title "A new" :directory "/a"
                           :tool reload-a :time-updated 3000)))))
          (roster-register-backend
           (roster-backend-create
            :id 'reload-b :label "B" :face 'default
            :load (lambda (_include-archived)
                    (push 'reload-b loaded)
                    (error "unrelated backend loaded"))))
          (roster-test--with-list-buffer
              '((:id "a-old" :title "A old" :directory "/a" :tool reload-a
                     :time-updated 2000)
                (:id "b-old" :title "B old" :directory "/b" :tool reload-b
                     :time-updated 1000))
            (setq-local roster-backend-source-p t)
            (roster--reload-tools '(reload-a))
            (should (equal loaded '(reload-a)))
            (should (equal (mapcar #'roster--session-id roster--all-sessions)
                           '("a-new" "b-old")))))
      (remhash 'reload-a roster--backends)
      (remhash 'reload-b roster--backends))))

(ert-deftest roster-reload-tools-keeps-old-data-when-reload-fails ()
  (unwind-protect
      (progn
        (roster-register-backend
         (roster-backend-create
          :id 'reload-fails :label "F" :face 'default
          :load (lambda (_include-archived) (error "temporary failure"))))
        (roster-test--with-list-buffer
            '((:id "old" :title "Old" :directory "/old" :tool reload-fails
                   :time-updated 1000))
          (setq-local roster-backend-source-p t)
          (roster--reload-tools '(reload-fails))
          (should (equal (mapcar #'roster--session-id roster--all-sessions)
                         '("old")))))
    (remhash 'reload-fails roster--backends)))

(ert-deftest roster-custom-source-falls-back-to-full-refresh-after-mutation ()
  (let ((sessions '((:id "one" :title "One" :directory "/one"
                         :tool opencode :time-updated 1000)))
        (loads 0))
    (with-temp-buffer
      (roster-mode)
      (setq-local roster-source-function
                  (lambda () (cl-incf loads) sessions))
      (roster--populate)
      (setq sessions '((:id "two" :title "Two" :directory "/two"
                            :tool opencode :time-updated 2000)))
      (roster--reload-tools '(opencode))
      (should (= loads 2))
      (should (equal (mapcar #'roster--session-id roster--all-sessions)
                     '("two"))))))

(ert-deftest roster-hard-refresh-runs-cache-clear-hook ()
  (with-temp-buffer
    (roster-mode)
    (let* ((clears 0)
           (reverts 0)
           (roster-clear-caches-hook
            (list (lambda () (cl-incf clears)))))
      (cl-letf (((symbol-function 'revert-buffer)
                 (lambda (&rest _) (cl-incf reverts))))
        (roster-refresh)
        (roster-refresh t))
      (should (= clears 1))
      (should (= reverts 2)))))

(ert-deftest roster-entry-includes-derived-columns ()
  (let* ((session '(:id "ses_1"
                        :title "Decklet Dev"
                        :directory "/tmp/decklet"
                        :tool opencode
                        :time-updated 1700000000000
                        :time-archived 1700000005000))
         (entry (roster--entry session))
         (columns (cadr entry)))
    (should (equal (car entry) '(opencode . "ses_1")))
    (should (string-match-p "Decklet Dev" (aref columns 0)))
    (should (equal (aref columns 1) "OC"))        ; tool tag
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

(ert-deftest roster-custom-backend-uses-registered-capabilities ()
  (let (renamed archived deleted)
    (unwind-protect
        (progn
          (roster-register-backend
           (roster-backend-create
            :id 'test-agent :label "TA" :face 'default
            :load (lambda (_include-archived) nil)
            :resume-command (lambda (_session) "test-agent resume")
            :new-command (lambda () "test-agent new")
            :rename (lambda (_session title) (setq renamed title))
            :archive (lambda (_session value) (setq archived value))
            :delete (lambda (_session) (setq deleted t))))
          (let ((session '(:id "t1" :title "Old" :directory "/tmp"
                               :tool test-agent)))
            (should (equal (roster--tool-label session) "TA"))
            (should (equal (roster--session-command session) "test-agent resume"))
            (should (equal (roster--new-session-command 'test-agent)
                           "test-agent new"))
            (cl-letf (((symbol-function 'roster--read-session-title)
                       (lambda (_session) "New"))
                      ((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (roster--rename-session-command session)
              (roster--set-archived-command session t)
              (roster--delete-session-command session))
            (should (equal renamed "New"))
            (should archived)
            (should deleted)))
      (remhash 'test-agent roster--backends))))

(ert-deftest roster-register-backend-rejects-invalid-capabilities ()
  (should-error
   (roster-register-backend
    (roster-backend-create :id 'bad :label "BAD" :face 'default :load t))))

(ert-deftest roster-new-session-ignores-backends-without-new-capability ()
  (let ((roster-enabled-tools '(browse-only)))
    (unwind-protect
        (progn
          (roster-register-backend
           (roster-backend-create
            :id 'browse-only :label "BO" :face 'default :load #'ignore))
          (should-error (roster--select-tool-for-new-session) :type 'user-error))
      (remhash 'browse-only roster--backends))))

(ert-deftest roster-bulk-operations-share-backend-batch-scope ()
  (let (visited
        (batch-count 0))
    (unwind-protect
        (progn
          (roster-register-backend
           (roster-backend-create
            :id 'batch-agent :label "BA" :face 'default :load #'ignore
            :batch (lambda (function)
                     (cl-incf batch-count)
                     (funcall function))))
          (roster--for-each-session-by-backend
           (lambda (session) (push (roster--session-id session) visited))
           '((:id "one" :tool batch-agent) (:id "two" :tool batch-agent)))
          (should (= batch-count 1))
          (should (equal (nreverse visited) '("one" "two"))))
      (remhash 'batch-agent roster--backends))))

(ert-deftest roster-move-rejects-backends-without-the-capability ()
  (should-error
   (roster--move-session-command
    '(:id "cx1" :title "Codex" :directory "/tmp" :tool codex))
   :type 'user-error))

;;; Terminal dispatch

(ert-deftest roster-available-terminal-options-filters-unavailable-entries ()
  (let ((roster-terminal-options
         '(("Always" ignore nil)
           ("Ready" forward-char roster-test--terminal-available-p)
           ("Missing" backward-char roster-test--terminal-unavailable-p))))
    (cl-letf (((symbol-function 'roster-test--terminal-available-p)
               (lambda () t))
              ((symbol-function 'roster-test--terminal-unavailable-p)
               (lambda () nil)))
      (should (equal (mapcar #'car (roster--available-terminal-options))
                     '("Always" "Ready"))))))

(ert-deftest roster-read-terminal-function-returns-selected-launcher ()
  (let ((roster-terminal-options
         '(("First" ignore nil) ("Second" forward-char nil))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Second")))
      (should (eq (roster--read-terminal-function) #'forward-char)))))

(ert-deftest roster-resume-session-accepts-terminal-override ()
  (let (called)
    (cl-letf (((symbol-function 'roster--session-directory)
               (lambda (_session) "/tmp/project"))
              ((symbol-function 'roster--session-command)
               (lambda (_session) "agent resume")))
      (roster--resume-session
       '(:id "one") nil
       (lambda (directory command) (setq called (list directory command))))
      (should (equal called '("/tmp/project" "agent resume"))))))

(ert-deftest roster-mode-binds-shift-return-to-terminal-selection ()
  (should (eq (lookup-key roster-mode-map (kbd "S-<return>"))
              #'roster-resume-with-terminal)))

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
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool opencode)
        (:id "s2" :title "B" :directory "/b" :time-updated 900 :tool opencode))
    (puthash '(opencode . "s1") t roster--marked)
    (puthash '(opencode . "s2") t roster--marked)
    (should (equal (sort (roster--marked-keys)
                         (lambda (a b) (string< (cdr a) (cdr b))))
                   '((opencode . "s1") (opencode . "s2"))))))

(ert-deftest roster-mark-toggles-on-and-off ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool opencode))
    ;; First m at point-min: marks the session and advances past it.
    (roster-mark)
    (should (gethash '(opencode . "s1") roster--marked))
    ;; Return to the session row and m again: should unmark.
    (goto-char (point-min))
    (roster-mark)
    (should-not (gethash '(opencode . "s1") roster--marked))))

(ert-deftest roster-unmark-removes-mark ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool opencode))
    (puthash '(opencode . "s1") t roster--marked)
    (roster-unmark)
    (should-not (gethash '(opencode . "s1") roster--marked))))

(ert-deftest roster-unmark-all-clears-all ()
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool opencode)
        (:id "s2" :title "B" :directory "/b" :time-updated 900 :tool opencode))
    (puthash '(opencode . "s1") t roster--marked)
    (puthash '(opencode . "s2") t roster--marked)
    (roster-unmark-all)
    (should (zerop (hash-table-count roster--marked)))))

(ert-deftest roster--nearest-surviving-session-prefers-forward ()
  "When two candidates are equidistant, prefer the one on a following line."
  (roster-test--with-list-buffer
      '((:id "s1" :title "A" :directory "/a" :time-updated 3000 :tool opencode)
        (:id "s2" :title "B" :directory "/b" :time-updated 2000 :tool opencode)
        (:id "s3" :title "C" :directory "/c" :time-updated 1000 :tool opencode))
    ;; Move to the middle row: sessions are sorted newest-first, so
    ;; s1 is line 1, s2 is line 2, s3 is line 3.
    (forward-line 1)
    (let ((nearest (roster--nearest-surviving-session
                    '((opencode . "s2")))))
      ;; s1 and s3 are equidistant; s3 follows so it should be preferred
      (should (equal nearest '(opencode . "s3"))))))

(ert-deftest roster-delete-calls-do-delete ()
  (let (deleted reloaded)
    (roster-test--with-list-buffer
        '((:id "s1" :title "A" :directory "/a" :time-updated 3000 :tool claude
               :encoded-dir "-a")
          (:id "s2" :title "B" :directory "/b" :time-updated 2000 :tool claude
               :encoded-dir "-b")
          (:id "s3" :title "C" :directory "/c" :time-updated 1000 :tool claude
               :encoded-dir "-c"))
      (forward-line 1)
      (puthash '(claude . "s2") t roster--marked)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'roster--do-delete-session)
                 (lambda (s) (push (plist-get s :id) deleted)))
                ((symbol-function 'roster--reload-tools)
                 (lambda (tools)
                   (setq reloaded tools)
                   (roster--redisplay-sessions)))
                ((symbol-function 'recenter) #'ignore))
        (roster-delete)
        (should (equal deleted '("s2")))
        (should (equal reloaded '(claude)))
        (should (equal (tabulated-list-get-id) '(claude . "s3")))
        (should (zerop (hash-table-count roster--marked)))))))

(ert-deftest roster-archive-calls-do-archive ()
  (let (archived-calls reloaded)
    (roster-test--with-list-buffer
        '((:id "s1" :title "A" :directory "/a" :time-updated 1000 :tool claude
               :encoded-dir "-a"))
      (puthash '(claude . "s1") t roster--marked)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'roster--do-archive-session)
                 (lambda (s a) (push (cons (plist-get s :id) a) archived-calls)))
                ((symbol-function 'roster--reload-tools)
                 (lambda (tools) (setq reloaded tools))))
        (roster-archive)
        ;; s1 is active (no :time-archived), so it should be archived (t)
        (should (equal archived-calls '(("s1" . t))))
        (should (equal reloaded '(claude)))
        (should (zerop (hash-table-count roster--marked)))))))

(provide 'roster-ui-test)

;;; roster-ui-test.el ends here
