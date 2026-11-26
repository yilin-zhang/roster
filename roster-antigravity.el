;;; roster-antigravity.el --- Antigravity CLI backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Antigravity CLI backend for roster.
;;
;; Session listing uses the official Agent Client Protocol (`agy acp') stdio
;; server.  Because ACP does not yet expose session mutations, delete removes
;; the local session files and cached index entries after checking
;; `presence/<id>.lock', archive is stored in a roster-only sidecar file, and
;; rename is left unsupported until Antigravity exposes a public API for it.

;;; Code:

(require 'roster-core)
(require 'cl-lib)
(require 'parse-time)

;;; Antigravity CLI backend

(defface roster-tool-antigravity-face
  '((t :inherit font-lock-constant-face))
  "Face for the Antigravity CLI tool tag in `roster' lists."
  :group 'roster)

(defcustom roster-antigravity-dir
  (expand-file-name "~/.gemini/antigravity-cli")
  "Path to the Antigravity CLI data directory."
  :type 'directory
  :group 'roster)

(defcustom roster-antigravity-command "agy"
  "Antigravity CLI executable name or full path."
  :type 'string
  :group 'roster)

(defcustom roster-antigravity-server-idle-seconds 300
  "Seconds an unused roster Antigravity ACP server remains alive.
Set to zero to keep it alive until Emacs exits."
  :type 'natnum
  :group 'roster)

(defconst roster--antigravity-rpc-timeout 15
  "Seconds to wait for an Antigravity ACP response.")

(defconst roster--antigravity-output-limit 65536
  "Maximum retained characters of Antigravity ACP stderr output.")

(defvar roster--antigravity-process nil)
(defvar roster--antigravity-stdout nil)
(defvar roster--antigravity-stderr nil)
(defvar roster--antigravity-request-id 0)
(defvar roster--antigravity-idle-timer nil)
(defvar roster--antigravity-use-depth 0)

;;; ACP server lifecycle

(defun roster--antigravity-server-command ()
  "Return the command used to start roster's Antigravity ACP server."
  (let* ((dir (directory-file-name (expand-file-name roster-antigravity-dir)))
         (gemini-dir (directory-file-name (file-name-directory dir)))
         (app-data-dir (file-name-nondirectory dir)))
    (list roster-antigravity-command "acp"
          "--gemini-dir" gemini-dir
          "--app-data-dir" app-data-dir)))

(defun roster--antigravity-trim-output (buffer)
  "Keep only recent Antigravity ACP diagnostics in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (> (buffer-size) roster--antigravity-output-limit)
        (delete-region (point-min)
                       (- (point-max) roster--antigravity-output-limit))))))

(defun roster--antigravity-cancel-idle-timer ()
  "Cancel the pending Antigravity ACP idle shutdown timer."
  (when (timerp roster--antigravity-idle-timer)
    (cancel-timer roster--antigravity-idle-timer))
  (setq roster--antigravity-idle-timer nil))

(defun roster--antigravity-stop-server ()
  "Stop roster's persistent Antigravity ACP server and clear its state."
  (let ((process roster--antigravity-process)
        (stdout roster--antigravity-stdout)
        (stderr roster--antigravity-stderr))
    (roster--antigravity-cancel-idle-timer)
    (setq roster--antigravity-process nil
          roster--antigravity-stdout nil
          roster--antigravity-stderr nil
          roster--antigravity-request-id 0)
    (when (process-live-p process)
      (delete-process process))
    (when (buffer-live-p stdout)
      (kill-buffer stdout))
    (when (buffer-live-p stderr)
      (kill-buffer stderr))))

(defun roster--antigravity-process-sentinel (process _event)
  "Clear persistent server state when Antigravity ACP PROCESS exits."
  (when (and (eq process roster--antigravity-process)
             (not (process-live-p process)))
    (roster--antigravity-stop-server)))

(defun roster--antigravity-server-ready-p ()
  "Return non-nil when the reusable Antigravity ACP server is ready."
  (and (process-live-p roster--antigravity-process)
       (buffer-live-p roster--antigravity-stdout)
       (equal (process-command roster--antigravity-process)
              (roster--antigravity-server-command))))

(defun roster--antigravity-touch-server ()
  "Reschedule shutdown of the reusable Antigravity ACP server."
  (roster--antigravity-cancel-idle-timer)
  (when (> roster-antigravity-server-idle-seconds 0)
    (setq roster--antigravity-idle-timer
          (run-at-time roster-antigravity-server-idle-seconds nil
                       #'roster--antigravity-stop-server))))

;;; ACP JSON-RPC protocol

(defun roster--antigravity-take-response (buffer request-id)
  "Extract and remove the JSON-RPC response for REQUEST-ID from BUFFER.
Return the decoded response plist, or nil if no complete matching line is
present yet."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (catch 'response
          (while (search-forward "\n" nil t)
            (let* ((line-end (1- (point)))
                   (line-start (save-excursion
                                 (goto-char line-end)
                                 (line-beginning-position)))
                   (line (buffer-substring-no-properties line-start line-end))
                   (obj (roster--read-json line)))
              (when (and obj (equal (plist-get obj :id) request-id))
                (delete-region line-start (point))
                (throw 'response obj))))
          nil)))))

(defun roster--antigravity-wait-response (process buffer request-id)
  "Wait for PROCESS to write the REQUEST-ID response into BUFFER."
  (let ((deadline (+ (float-time) roster--antigravity-rpc-timeout))
        response)
    (while (and (not response)
                (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.05)
      (roster--antigravity-trim-output roster--antigravity-stderr)
      (setq response (roster--antigravity-take-response buffer request-id)))
    (or response
        (let* ((detail (when (buffer-live-p roster--antigravity-stderr)
                         (with-current-buffer roster--antigravity-stderr
                           (string-trim (buffer-string)))))
               (reason (if (process-live-p process)
                           "timed out"
                         "exited before replying")))
          (user-error "Antigravity ACP server %s%s"
                      reason
                      (if (string-empty-p (or detail ""))
                          ""
                        (format ": %.500s" detail)))))))

(defun roster--antigravity-send-rpc (process method request-id params)
  "Send METHOD with REQUEST-ID and PARAMS to Antigravity ACP PROCESS."
  (process-send-string
   process
   (concat (json-encode `(("jsonrpc" . "2.0")
                          ("id" . ,request-id)
                          ("method" . ,method)
                          ("params" . ,(or params (make-hash-table)))))
           "\n")))

(defun roster--antigravity-check-response (response)
  "Return RESPONSE result, or signal its Antigravity ACP error."
  (when-let* ((error-object (plist-get response :error)))
    (let ((message (or (plist-get error-object :message)
                       (format "%s" error-object))))
      (if (string-match-p "Authentication required" message)
          (user-error "Antigravity CLI requires sign-in; run `%s' first"
                      roster-antigravity-command)
        (user-error "Antigravity ACP error: %s" message))))
  (plist-get response :result))

(defun roster--antigravity-request-on-connection (method &optional params)
  "Call METHOD with PARAMS on the active Antigravity ACP connection."
  (let ((request-id (cl-incf roster--antigravity-request-id)))
    (roster--antigravity-send-rpc
     roster--antigravity-process method request-id params)
    (roster--antigravity-check-response
     (roster--antigravity-wait-response
      roster--antigravity-process roster--antigravity-stdout request-id))))

(defun roster--antigravity-start-server ()
  "Start and initialize roster's persistent Antigravity ACP server."
  (roster--antigravity-stop-server)
  (let ((stdout (generate-new-buffer " *roster-antigravity-stdout*"))
        (stderr (generate-new-buffer " *roster-antigravity-stderr*"))
        (command (roster--antigravity-server-command))
        process)
    (condition-case err
        (progn
          (setq process
                (make-process
                 :name "roster-antigravity-acp"
                 :buffer stdout
                 :stderr stderr
                 :command command
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :sentinel #'roster--antigravity-process-sentinel
                 :noquery t))
          (setq roster--antigravity-process process
                roster--antigravity-stdout stdout
                roster--antigravity-stderr stderr
                roster--antigravity-request-id 0)
          (roster--antigravity-request-on-connection
           "initialize"
           '(("protocolVersion" . 1)
             ("clientInfo" . (("name" . "roster")
                              ("version" . "0.2.0")))))
          process)
      (error
       (if (eq process roster--antigravity-process)
           (roster--antigravity-stop-server)
         (when (process-live-p process)
           (delete-process process))
         (when (buffer-live-p stdout)
           (kill-buffer stdout))
         (when (buffer-live-p stderr)
           (kill-buffer stderr)))
       (signal (car err) (cdr err))))))

(defun roster--antigravity-call-with-server (function)
  "Call FUNCTION while the reusable Antigravity ACP server is available."
  (roster--antigravity-cancel-idle-timer)
  (unless (roster--antigravity-server-ready-p)
    (roster--antigravity-start-server))
  (cl-incf roster--antigravity-use-depth)
  (condition-case err
      (unwind-protect
          (funcall function)
        (cl-decf roster--antigravity-use-depth)
        (when (zerop roster--antigravity-use-depth)
          (roster--antigravity-trim-output roster--antigravity-stderr)
          (roster--antigravity-touch-server)))
    (error
     ;; Retire a suspect transport so the next action starts cleanly.
     (when (zerop roster--antigravity-use-depth)
       (roster--antigravity-stop-server))
     (signal (car err) (cdr err)))))

(defun roster--antigravity-acp-request (method &optional params)
  "Call Antigravity ACP METHOD synchronously with PARAMS."
  (roster--antigravity-call-with-server
   (lambda () (roster--antigravity-request-on-connection method params))))

(add-hook 'kill-emacs-hook #'roster--antigravity-stop-server)
(add-hook 'roster-clear-caches-hook #'roster--antigravity-stop-server)

;;; Local paths and sidecars

(defun roster--antigravity-validate-id (session-id)
  "Validate SESSION-ID as a safe single path component and return it."
  (unless (and (stringp session-id)
               (not (string-empty-p session-id))
               (not (string-match-p "[/\\\\]" session-id))
               (not (member session-id '("." ".."))))
    (user-error "Invalid Antigravity session id: %S" session-id))
  session-id)

(defun roster--antigravity-roster-dir ()
  "Return the directory used to store Antigravity roster sidecar files."
  (expand-file-name "roster" roster-antigravity-dir))

(defun roster--antigravity-sidecar-path (session-id)
  "Return the path to the roster sidecar JSON file for SESSION-ID."
  (expand-file-name (concat (roster--antigravity-validate-id session-id)
                            ".roster.json")
                    (roster--antigravity-roster-dir)))

(defun roster--antigravity-annotation-path (session-id)
  "Return the path to Antigravity's annotation `.pbtxt' for SESSION-ID."
  (expand-file-name (concat (roster--antigravity-validate-id session-id)
                            ".pbtxt")
                    (expand-file-name "annotations" roster-antigravity-dir)))

(defun roster--antigravity-conversation-db-path (session-id)
  "Return the path to Antigravity's conversation SQLite file for SESSION-ID."
  (expand-file-name (concat (roster--antigravity-validate-id session-id) ".db")
                    (expand-file-name "conversations" roster-antigravity-dir)))

(defun roster--antigravity-brain-dir (session-id)
  "Return the path to Antigravity's brain directory for SESSION-ID."
  (expand-file-name (roster--antigravity-validate-id session-id)
                    (expand-file-name "brain" roster-antigravity-dir)))

(defun roster--antigravity-lock-path (session-id)
  "Return the path to Antigravity's presence lock file for SESSION-ID."
  (expand-file-name (concat (roster--antigravity-validate-id session-id) ".lock")
                    (expand-file-name "presence" roster-antigravity-dir)))

(defun roster--antigravity-summaries-db-path ()
  "Return the path to Antigravity's `conversation_summaries.db'."
  (expand-file-name "conversation_summaries.db" roster-antigravity-dir))

(defun roster--antigravity-cache-path (filename)
  "Return the path to FILENAME under Antigravity's `cache' directory."
  (expand-file-name filename
                    (expand-file-name "cache" roster-antigravity-dir)))

(defun roster--antigravity-time-archived (session-id)
  "Return roster's archive time for Antigravity SESSION-ID, or nil."
  (let ((value (cdr (assoc "time_archived"
                           (roster--read-sidecar
                            (roster--antigravity-sidecar-path session-id))))))
    (when (numberp value) value)))

;;; Session loading

(defun roster--antigravity-parse-timestamp (timestamp)
  "Convert RFC 3339 TIMESTAMP string to milliseconds since epoch, or 0."
  (if (and (stringp timestamp) (not (string-empty-p timestamp)))
      (condition-case nil
          (if-let* ((parsed (parse-iso8601-time-string timestamp)))
              (floor (* roster--ms-per-second (float-time parsed)))
            0)
        (error 0))
    0))

(defun roster--antigravity-session-from-api (value)
  "Return unified Antigravity session parsed from ACP VALUE."
  (when-let* ((session-id (plist-get value :sessionId))
              ((stringp session-id))
              ((not (string-empty-p session-id))))
    (let* ((title (plist-get value :title))
           (cwd (plist-get value :cwd))
           (updated-at (plist-get value :updatedAt)))
      (list :id session-id
            :title (if (and (stringp title) (not (string-empty-p title)))
                       (roster--truncate-title title)
                     roster--untitled)
            :directory (expand-file-name
                        (if (and (stringp cwd) (not (string-empty-p cwd)))
                            cwd
                          "~"))
            :project-id nil
            :time-updated (roster--antigravity-parse-timestamp updated-at)
            :time-archived (roster--antigravity-time-archived session-id)
            :tool 'antigravity))))

(defun roster--antigravity-list-page (&optional cursor)
  "Return one Antigravity ACP `session/list' page for optional CURSOR."
  (roster--antigravity-acp-request
   "session/list"
   (when cursor `(("cursor" . ,cursor)))))

(defun roster--antigravity-load-sessions (&optional include-archived)
  "Return Antigravity sessions through ACP.
Include archived sessions only when INCLUDE-ARCHIVED is non-nil."
  (let ((sessions
         (roster--antigravity-call-with-server
          (lambda ()
            (let (result cursor)
              (while
                  (let* ((page (roster--antigravity-list-page cursor))
                         (items (plist-get page :sessions)))
                    (dolist (item items)
                      (when-let* ((session
                                   (roster--antigravity-session-from-api item)))
                        (push session result)))
                    (setq cursor (plist-get page :nextCursor))))
              (nreverse result))))))
    (if include-archived sessions (roster--active-sessions sessions))))

;;; Active session detection and local mutations

(defun roster--antigravity-session-open-p (session-id)
  "Return non-nil when Antigravity SESSION-ID is locked by a running client."
  (let ((lock-path (roster--antigravity-lock-path session-id)))
    (when (file-exists-p lock-path)
      (cond
       ((executable-find "perl")
        (eq 1 (call-process
               "perl" nil nil nil
               "-MFcntl=:flock" "-MErrno=EAGAIN,EACCES,EWOULDBLOCK" "-e"
               (concat "open my $fh, '+<', $ARGV[0] or exit 0; "
                       "exit 0 if flock($fh, LOCK_EX | LOCK_NB); "
                       "exit($!{EAGAIN} || $!{EACCES} || $!{EWOULDBLOCK} ? 1 : 0);")
               lock-path)))
       ((executable-find "python3")
        (eq 1 (call-process
               "python3" nil nil nil "-c"
               (concat "import errno, fcntl, sys\n"
                       "try:\n"
                       "  f = open(sys.argv[1], 'r+')\n"
                       "except OSError:\n"
                       "  sys.exit(0)\n"
                       "try:\n"
                       "  fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)\n"
                       "except OSError as e:\n"
                       "  sys.exit(1 if e.errno in (errno.EAGAIN, errno.EACCES) else 0)\n"
                       "sys.exit(0)\n")
               lock-path)))))))

(defun roster--antigravity-assert-not-open (session-id)
  "Signal a `user-error' if Antigravity SESSION-ID is open in a running client."
  (when (roster--antigravity-session-open-p session-id)
    (user-error
     "Antigravity session %s is still open; close its agy client, then retry"
     session-id)))

(defun roster--antigravity-do-archive (session archived)
  "Set Antigravity SESSION archived state to ARCHIVED without prompting."
  ;; Antigravity ACP does not expose archive metadata, so roster stores archive
  ;; state in a sidecar file, matching the Claude Code and pi backends.
  (roster--write-sidecar
   (roster--antigravity-sidecar-path (roster--session-id session))
   nil
   (when archived
     (floor (* roster--ms-per-second (float-time (current-time)))))))

(defun roster--antigravity-prune-summary-db (session-id)
  "Remove SESSION-ID rows from `conversation_summaries.db' on a best-effort basis."
  (let ((db-path (roster--antigravity-summaries-db-path)))
    (when (and (file-writable-p db-path)
               (require 'sqlite nil t)
               (sqlite-available-p))
      (condition-case nil
          (let ((db (sqlite-open db-path)))
            (unwind-protect
                (progn
                  (sqlite-execute db "PRAGMA busy_timeout = 1000")
                  (sqlite-execute
                   db "DELETE FROM conversation_summaries WHERE conversation_id = ?"
                   (vector session-id))
                  (sqlite-execute
                   db "DELETE FROM conversations_fts WHERE conversation_id = ?"
                   (vector session-id))
                  (sqlite-execute
                   db
                   (concat "DELETE FROM conversation_parents "
                           "WHERE conversation_id = ? OR parent_id = ?")
                   (vector session-id session-id)))
              (sqlite-close db)))
        (error nil)))))

(defun roster--antigravity-update-json-cache (path updater)
  "Rewrite JSON cache file at PATH by passing its root hash table to UPDATER.
Only rewrites PATH when UPDATER returns non-nil.  Errors are ignored because
these files are derived caches."
  (when (and (json-available-p)
             (file-readable-p path)
             (file-writable-p path))
    (condition-case nil
        (let ((data (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8))
                        (insert-file-contents path))
                      (json-parse-buffer :object-type 'hash-table))))
          (when (and (hash-table-p data)
                     (funcall updater data))
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file path
                (insert (json-serialize data))))))
      (error nil))))

(defun roster--antigravity-prune-caches (session-id)
  "Remove SESSION-ID from Antigravity's local JSON caches on a best-effort basis."
  (roster--antigravity-update-json-cache
   (roster--antigravity-cache-path "conversation_metadata.json")
   (lambda (root)
     (let ((conversations (gethash "conversations" root)))
       (when (and (hash-table-p conversations)
                  (gethash session-id conversations))
         (remhash session-id conversations)
         t))))
  (roster--antigravity-update-json-cache
   (roster--antigravity-cache-path "last_conversations.json")
   (lambda (root)
     (let (stale)
       (maphash (lambda (workspace id)
                  (when (equal id session-id)
                    (push workspace stale)))
                root)
       (dolist (workspace stale)
         (remhash workspace root))
       (not (null stale))))))

(defun roster--antigravity-delete-session (session)
  "Delete Antigravity SESSION's local files, cache entries, and sidecar.
Refuse deletion when `presence/<id>.lock' indicates a running client."
  (let* ((session-id (roster--antigravity-validate-id
                      (roster--session-id session)))
         (db-path (roster--antigravity-conversation-db-path session-id))
         (annotation-path (roster--antigravity-annotation-path session-id))
         (lock-path (roster--antigravity-lock-path session-id))
         (brain-dir (roster--antigravity-brain-dir session-id))
         (sidecar (roster--antigravity-sidecar-path session-id)))
    (roster--antigravity-assert-not-open session-id)
    (dolist (path (list db-path
                        (concat db-path "-wal")
                        (concat db-path "-shm")
                        annotation-path
                        lock-path
                        sidecar))
      (when (file-exists-p path)
        (delete-file path)))
    (when (file-directory-p brain-dir)
      (delete-directory brain-dir t))
    (roster--antigravity-prune-summary-db session-id)
    (roster--antigravity-prune-caches session-id)))

(defun roster--antigravity-resume-command (session)
  "Return the shell command used to resume Antigravity SESSION."
  (format "%s -r %s" roster-antigravity-command
          (shell-quote-argument (roster--session-id session))))

(defun roster--antigravity-new-command ()
  "Return the shell command used to start an Antigravity CLI session."
  roster-antigravity-command)

(roster-register-backend
 (roster-backend-create
  :id 'antigravity
  :label "AG"
  :face 'roster-tool-antigravity-face
  :load #'roster--antigravity-load-sessions
  :resume-command #'roster--antigravity-resume-command
  :new-command #'roster--antigravity-new-command
  :archive #'roster--antigravity-do-archive
  :delete #'roster--antigravity-delete-session
  :batch #'roster--antigravity-call-with-server))

(provide 'roster-antigravity)

;;; roster-antigravity.el ends here
