;;; roster-antigravity-test.el --- Antigravity CLI tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Antigravity CLI backend.

;;; Code:

(require 'roster-test-helpers)

(defmacro roster-antigravity-test-with-root (&rest body)
  "Run BODY with `roster-antigravity-dir' bound to a fresh temporary directory."
  (declare (indent 0))
  `(let* ((root (make-temp-file "roster-antigravity-root-" t))
          (roster-antigravity-dir root))
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

;;; ACP server lifecycle and protocol

(ert-deftest roster-antigravity-server-command-derives-gemini-and-app-dirs ()
  (let ((roster-antigravity-command "agy")
        (roster-antigravity-dir "/tmp/custom-gemini/antigravity-cli/"))
    (should (equal (roster--antigravity-server-command)
                   '("agy" "acp"
                     "--gemini-dir" "/tmp/custom-gemini"
                     "--app-data-dir" "antigravity-cli")))))

(ert-deftest roster-antigravity-call-with-server-reuses-ready-process ()
  (let ((ready nil)
        (starts 0)
        (touches 0)
        (roster--antigravity-idle-timer nil)
        (roster--antigravity-use-depth 0))
    (cl-letf (((symbol-function 'roster--antigravity-server-ready-p)
               (lambda () ready))
              ((symbol-function 'roster--antigravity-start-server)
               (lambda () (cl-incf starts) (setq ready t)))
              ((symbol-function 'roster--antigravity-trim-output) #'ignore)
              ((symbol-function 'roster--antigravity-touch-server)
               (lambda () (cl-incf touches))))
      (should (eq (roster--antigravity-call-with-server (lambda () 'first))
                  'first))
      (should (eq (roster--antigravity-call-with-server (lambda () 'second))
                  'second))
      (should (= starts 1))
      (should (= touches 2)))))

(ert-deftest roster-antigravity-nested-server-calls-manage-one-idle-window ()
  (let ((touches 0)
        (roster--antigravity-idle-timer
         (run-at-time 3600 nil #'ignore))
        (roster--antigravity-use-depth 0))
    (unwind-protect
        (cl-letf (((symbol-function 'roster--antigravity-server-ready-p)
                   (lambda () t))
                  ((symbol-function 'roster--antigravity-trim-output) #'ignore)
                  ((symbol-function 'roster--antigravity-touch-server)
                   (lambda () (cl-incf touches))))
          (roster--antigravity-call-with-server
           (lambda ()
             (should-not roster--antigravity-idle-timer)
             (roster--antigravity-call-with-server
              (lambda () (should (= roster--antigravity-use-depth 2))))))
          (should (= touches 1))
          (should (zerop roster--antigravity-use-depth)))
      (roster--antigravity-cancel-idle-timer))))

(ert-deftest roster-antigravity-call-with-server-stops-on-error ()
  (let ((stops 0)
        (roster--antigravity-idle-timer nil)
        (roster--antigravity-use-depth 0))
    (cl-letf (((symbol-function 'roster--antigravity-server-ready-p)
               (lambda () t))
              ((symbol-function 'roster--antigravity-trim-output) #'ignore)
              ((symbol-function 'roster--antigravity-touch-server) #'ignore)
              ((symbol-function 'roster--antigravity-stop-server)
               (lambda () (cl-incf stops))))
      (should-error
       (roster--antigravity-call-with-server (lambda () (error "Boom"))))
      (should (= stops 1))
      (should (zerop roster--antigravity-use-depth)))))

(ert-deftest roster-antigravity-take-response-extracts-matching-line ()
  (with-temp-buffer
    (insert "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{}}\n"
            "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"sessions\":[]}}\n"
            "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"ok\":true}}\n")
    (let ((response (roster--antigravity-take-response (current-buffer) 2)))
      (should (equal (plist-get response :id) 2))
      (should-not (string-match-p "\"id\":2" (buffer-string)))
      (should (string-match-p "\"id\":3" (buffer-string))))))

(ert-deftest roster-antigravity-check-response-signals-auth-hint ()
  (let ((roster-antigravity-command "agy"))
    (let ((err (should-error
                (roster--antigravity-check-response
                 '(:id 2 :error (:code -32000 :message "Authentication required")))
                :type 'user-error)))
      (should (string-match-p "agy" (error-message-string err)))))
  (should-error
   (roster--antigravity-check-response
    '(:id 3 :error (:code -32603 :message "internal failure")))
   :type 'user-error))

;;; Session loading

(ert-deftest roster-antigravity-parse-timestamp-handles-rfc3339 ()
  (should (= (roster--antigravity-parse-timestamp "2026-11-26T05:10:09Z")
             1795669809000))
  (should (= (roster--antigravity-parse-timestamp "2026-11-26T05:10:09.500Z")
             1795669809000))
  (should (= (roster--antigravity-parse-timestamp nil) 0))
  (should (= (roster--antigravity-parse-timestamp "not-a-time") 0)))

(ert-deftest roster-antigravity-session-from-api-maps-fields ()
  (roster-antigravity-test-with-root
    (let ((session
           (roster--antigravity-session-from-api
            '(:sessionId "ag-1" :title "Antigravity Task"
                         :cwd "/tmp/agy" :updatedAt "2026-11-26T05:10:09Z"))))
      (should (equal (plist-get session :id) "ag-1"))
      (should (equal (plist-get session :title) "Antigravity Task"))
      (should (equal (plist-get session :directory) "/tmp/agy"))
      (should (= (plist-get session :time-updated) 1795669809000))
      (should-not (plist-get session :time-archived))
      (should (eq (plist-get session :tool) 'antigravity)))))

(ert-deftest roster-antigravity-session-from-api-handles-missing-fields ()
  "Long titles are truncated; missing titles and directories get defaults."
  (roster-antigravity-test-with-root
    (let ((long (roster--antigravity-session-from-api
                 (list :sessionId "long" :title (make-string 100 ?x)
                       :cwd "/tmp")))
          (bare (roster--antigravity-session-from-api
                 (list :sessionId "bare" :title "" :cwd nil))))
      (should (= (length (plist-get long :title)) roster--title-max-length))
      (should (equal (plist-get bare :title) roster--untitled))
      (should (equal (plist-get bare :directory) (expand-file-name "~")))
      (should (= (plist-get bare :time-updated) 0)))))

(ert-deftest roster-antigravity-load-sessions-paginates-and-filters-archived ()
  (roster-antigravity-test-with-root
    (roster--antigravity-do-archive '(:id "ag-old" :tool antigravity) t)
    (let (cursors)
      (cl-letf (((symbol-function 'roster--antigravity-call-with-server)
                 (lambda (function) (funcall function)))
                ((symbol-function 'roster--antigravity-list-page)
                 (lambda (&optional cursor)
                   (push cursor cursors)
                   (if cursor
                       '(:sessions ((:sessionId "ag-old" :title "Old"
                                                :cwd "/b"
                                                :updatedAt "2026-11-26T05:00:00Z")))
                     '(:sessions ((:sessionId "ag-new" :title "New"
                                              :cwd "/a"
                                              :updatedAt "2026-11-26T05:10:00Z"))
                                 :nextCursor "page-2")))))
        (should (equal (mapcar #'roster--session-id
                               (roster--antigravity-load-sessions))
                       '("ag-new")))
        (should (equal (nreverse cursors) '(nil "page-2")))
        (let ((all (roster--antigravity-load-sessions t)))
          (should (equal (mapcar #'roster--session-id all)
                         '("ag-new" "ag-old")))
          (should (roster--session-archived-p (cadr all))))))))

;;; Local mutations and commands

(ert-deftest roster-antigravity-rename-is-unsupported ()
  (should-not (roster-backend-rename (roster--backend 'antigravity)))
  (cl-letf (((symbol-function 'roster--read-session-title)
             (lambda (_session) "New title")))
    (should-error
     (roster--rename-session-command
      '(:id "ag-1" :title "Old title" :tool antigravity))
     :type 'user-error)))

(ert-deftest roster-antigravity-archive-round-trips-through-sidecar ()
  (roster-antigravity-test-with-root
    (let ((session '(:id "ag-1" :tool antigravity)))
      (roster--antigravity-do-archive session t)
      (should (numberp (roster--antigravity-time-archived "ag-1")))
      (roster--antigravity-do-archive session nil)
      (should-not (roster--antigravity-time-archived "ag-1")))))

(ert-deftest roster-antigravity-delete-removes-local-files-and-caches ()
  (roster-antigravity-test-with-root
    (let* ((session '(:id "ag-1" :tool antigravity))
           (db-path (roster--antigravity-conversation-db-path "ag-1"))
           (wal-path (concat db-path "-wal"))
           (shm-path (concat db-path "-shm"))
           (annotation-path (roster--antigravity-annotation-path "ag-1"))
           (lock-path (roster--antigravity-lock-path "ag-1"))
           (brain-dir (roster--antigravity-brain-dir "ag-1"))
           (sidecar (roster--antigravity-sidecar-path "ag-1"))
           (summaries-path (roster--antigravity-summaries-db-path))
           (metadata-cache
            (roster--antigravity-cache-path "conversation_metadata.json"))
           (last-cache
            (roster--antigravity-cache-path "last_conversations.json")))
      (dolist (dir (list (file-name-directory db-path)
                         (file-name-directory annotation-path)
                         (file-name-directory lock-path)
                         (file-name-directory metadata-cache)
                         brain-dir))
        (make-directory dir t))
      (dolist (file (list db-path wal-path shm-path annotation-path lock-path
                          (expand-file-name "artifact.md" brain-dir)))
        (with-temp-file file (insert "data")))
      (with-temp-file metadata-cache
        (insert "{\"conversations\":{\"ag-1\":{\"is_internal\":false},"
                "\"ag-2\":{\"is_internal\":false,\"summary\":{\"Title\":\"Keep\"}}}}"))
      (with-temp-file last-cache
        (insert "{\"/tmp/one\":\"ag-1\",\"/tmp/two\":\"ag-2\"}"))
      (roster--antigravity-do-archive session t)
      (when (and (require 'sqlite nil t) (sqlite-available-p))
        (let ((db (sqlite-open summaries-path)))
          (unwind-protect
              (progn
                (sqlite-execute
                 db "CREATE TABLE conversation_summaries (conversation_id TEXT)")
                (sqlite-execute
                 db "CREATE TABLE conversations_fts (conversation_id TEXT)")
                (sqlite-execute
                 db "CREATE TABLE conversation_parents (conversation_id TEXT, parent_id TEXT)")
                (sqlite-execute
                 db "INSERT INTO conversation_summaries VALUES ('ag-1'), ('ag-2')")
                (sqlite-execute
                 db "INSERT INTO conversations_fts VALUES ('ag-1'), ('ag-2')")
                (sqlite-execute
                 db "INSERT INTO conversation_parents VALUES ('ag-1', 'p'), ('c', 'ag-1'), ('ag-2', 'p')"))
            (sqlite-close db))))
      (cl-letf (((symbol-function 'roster--antigravity-session-open-p)
                 (lambda (_id) nil)))
        (roster--antigravity-delete-session session))
      (dolist (path (list db-path wal-path shm-path annotation-path lock-path
                          brain-dir sidecar))
        (should-not (file-exists-p path)))
      (when (json-available-p)
        (let ((meta (with-temp-buffer
                      (insert-file-contents metadata-cache)
                      (json-parse-buffer :object-type 'hash-table)))
              (last (with-temp-buffer
                      (insert-file-contents last-cache)
                      (json-parse-buffer :object-type 'hash-table))))
          (should-not (gethash "ag-1" (gethash "conversations" meta)))
          (should (gethash "ag-2" (gethash "conversations" meta)))
          (should-not (gethash "/tmp/one" last))
          (should (equal (gethash "/tmp/two" last) "ag-2"))))
      (when (and (require 'sqlite nil t) (sqlite-available-p))
        (let ((db (sqlite-open summaries-path)))
          (unwind-protect
              (progn
                (should (equal (sqlite-select
                                db "SELECT conversation_id FROM conversation_summaries")
                               '(("ag-2"))))
                (should (equal (sqlite-select
                                db "SELECT conversation_id FROM conversations_fts")
                               '(("ag-2"))))
                (should (equal (sqlite-select
                                db "SELECT conversation_id FROM conversation_parents")
                               '(("ag-2")))))
            (sqlite-close db)))))))

(ert-deftest roster-antigravity-delete-refuses-open-session ()
  (roster-antigravity-test-with-root
    (let* ((session '(:id "ag-1" :tool antigravity))
           (db-path (roster--antigravity-conversation-db-path "ag-1")))
      (make-directory (file-name-directory db-path) t)
      (with-temp-file db-path (insert "keep"))
      (cl-letf (((symbol-function 'roster--antigravity-session-open-p)
                 (lambda (_id) t)))
        (let ((err (should-error (roster--antigravity-delete-session session)
                                 :type 'user-error)))
          (should (string-match-p "still open" (error-message-string err)))))
      (should (file-exists-p db-path)))))

(ert-deftest roster-antigravity-validate-id-rejects-path-traversal ()
  (should-error (roster--antigravity-validate-id "../escape") :type 'user-error)
  (should-error (roster--antigravity-validate-id "a/b") :type 'user-error)
  (should-error (roster--antigravity-validate-id "") :type 'user-error))

(ert-deftest roster-antigravity-commands-use-configured-binary ()
  (let ((roster-antigravity-command "agy"))
    (should (equal (roster--antigravity-new-command) "agy"))
    (should (equal (roster--antigravity-resume-command '(:id "abc-123"))
                   "agy -r abc-123"))))

(provide 'roster-antigravity-test)

;;; roster-antigravity-test.el ends here
