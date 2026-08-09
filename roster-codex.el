;;; roster-codex.el --- Codex backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Codex backend using the official app-server API.

;;; Code:

(require 'roster-core)
(require 'cl-lib)

(defface roster-tool-codex-face
  '((t :inherit success))
  "Face for the Codex tool tag in `roster' lists."
  :group 'roster)

(defcustom roster-codex-dir
  (expand-file-name "~/.codex")
  "Path to the Codex configuration directory."
  :type 'directory
  :group 'roster)

(defcustom roster-codex-command "codex"
  "Codex executable name or full path."
  :type 'string
  :group 'roster)

(defconst roster--codex-rpc-timeout 5
  "Seconds to wait for a Codex app-server response.")

(defvar roster--codex-process nil)
(defvar roster--codex-stdout nil)
(defvar roster--codex-stderr nil)
(defvar roster--codex-request-id 0)
(defvar roster--codex-legacy-titles nil)

;;; Legacy metadata

(defun roster--codex-roster-dir ()
  "Return the directory containing deprecated Codex roster sidecars."
  (expand-file-name "roster" roster-codex-dir))

(defun roster--codex-sidecar-path (session-id)
  "Return deprecated roster sidecar path for Codex SESSION-ID."
  (expand-file-name (concat session-id ".roster.json")
                    (roster--codex-roster-dir)))

(defun roster--codex-read-sidecar (session-id)
  "Return deprecated roster metadata for Codex SESSION-ID, or nil.
Compatibility path for names written before roster used Codex's native
thread metadata and app-server API.  A successful native rename removes
the corresponding sidecar."
  (roster--read-sidecar (roster--codex-sidecar-path session-id)))

(defun roster--codex-delete-legacy-sidecar (session-id)
  "Delete deprecated roster sidecar for Codex SESSION-ID when present."
  (let ((path (roster--codex-sidecar-path session-id)))
    (when (file-exists-p path)
      (delete-file path))))

(defun roster--codex-load-legacy-titles ()
  "Return an id-to-title table for deprecated Codex sidecars."
  (let ((table (make-hash-table :test #'equal))
        (directory (roster--codex-roster-dir)))
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t "\\.roster\\.json\\'"))
        (when-let ((title (cdr (assoc "title" (roster--read-sidecar path)))))
          (puthash (string-remove-suffix
                    ".roster.json" (file-name-nondirectory path))
                   title table))))
    table))

(defun roster--codex-legacy-title (session-id)
  "Return deprecated sidecar title for SESSION-ID."
  (if (hash-table-p roster--codex-legacy-titles)
      (gethash session-id roster--codex-legacy-titles)
    (cdr (assoc "title" (roster--codex-read-sidecar session-id)))))

;;; App-server protocol

(defun roster--codex-json-response (buffer request-id)
  "Return response for REQUEST-ID found in JSONL BUFFER, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (catch 'response
        (while (not (eobp))
          (when-let ((obj (roster--read-json
                           (buffer-substring-no-properties
                            (point) (line-end-position)))))
            (when (equal (plist-get obj :id) request-id)
              (throw 'response obj)))
          (forward-line 1))
        nil))))

(defun roster--codex-wait-response (process buffer request-id)
  "Wait for PROCESS to write REQUEST-ID response into BUFFER."
  (let ((deadline (+ (float-time) roster--codex-rpc-timeout))
        response)
    (while (and (not response)
                (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.05)
      (setq response (roster--codex-json-response buffer request-id)))
    (or response
        (let* ((detail (when (buffer-live-p roster--codex-stderr)
                         (with-current-buffer roster--codex-stderr
                           (string-trim (buffer-string)))))
               (reason (if (process-live-p process)
                           "timed out"
                         "exited before replying")))
          (user-error "Codex app-server %s%s"
                      reason
                      (if (string-empty-p (or detail ""))
                          ""
                        (format ": %.500s" detail)))))))

(defun roster--codex-send-rpc (process method request-id params)
  "Send METHOD with REQUEST-ID and PARAMS to Codex app-server PROCESS."
  (process-send-string
   process
   (concat (json-encode `(("method" . ,method)
                          ("id" . ,request-id)
                          ("params" . ,params)))
           "\n")))

(defun roster--codex-check-response (response)
  "Return RESPONSE result, or signal its Codex app-server error."
  (when-let ((error-object (plist-get response :error)))
    (user-error "Codex app-server error: %s"
                (or (plist-get error-object :message) error-object)))
  (plist-get response :result))

(defun roster--codex-request-on-connection (method params)
  "Call METHOD with PARAMS on the current scoped app-server connection."
  (let ((request-id (cl-incf roster--codex-request-id)))
    (roster--codex-send-rpc roster--codex-process method request-id params)
    (roster--codex-check-response
     (roster--codex-wait-response roster--codex-process
                                  roster--codex-stdout request-id))))

(defun roster--codex-call-with-app-server (function)
  "Call FUNCTION while one initialized Codex app-server is available."
  (let ((stdout (generate-new-buffer " *roster-codex-stdout*"))
        (stderr (generate-new-buffer " *roster-codex-stderr*"))
        process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "roster-codex-app-server"
                 :buffer stdout
                 :stderr stderr
                 :command (list roster-codex-command
                                "app-server" "--listen" "stdio://")
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t))
          (let ((roster--codex-process process)
                (roster--codex-stdout stdout)
                (roster--codex-stderr stderr)
                (roster--codex-request-id 0))
            (roster--codex-request-on-connection
             "initialize"
             '(("clientInfo" . (("name" . "roster")
                                ("title" . "Roster")
                                ("version" . "0.2.0")))))
            (process-send-string process
                                 "{\"method\":\"initialized\",\"params\":{}}\n")
            (funcall function)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer stdout)
      (kill-buffer stderr))))

(defun roster--codex-app-server-request (method params)
  "Call Codex app-server METHOD synchronously with PARAMS.
Reuse the dynamically scoped connection during loads and bulk operations."
  (if (and roster--codex-process (process-live-p roster--codex-process))
      (roster--codex-request-on-connection method params)
    (roster--codex-call-with-app-server
     (lambda () (roster--codex-request-on-connection method params)))))

;;; Session loading

(defun roster--codex-thread-from-api (thread archived)
  "Return unified session for Codex THREAD.
ARCHIVED is non-nil when THREAD came from the archived listing."
  (when-let* ((session-id (plist-get thread :id))
              ((stringp session-id)))
    (let* ((legacy-title (roster--codex-legacy-title session-id))
           (cwd (plist-get thread :cwd))
           (directory (expand-file-name
                       (if (and (stringp cwd) (not (string-empty-p cwd)))
                           cwd
                         "~")))
           (name (plist-get thread :name))
           (preview (plist-get thread :preview))
           (updated-at (plist-get thread :updatedAt)))
      (list :id session-id
            ;; Compatibility: preserve explicit legacy roster names until the
            ;; next native rename removes their sidecar.
            :title (or legacy-title
                       (and (stringp name) (not (string-empty-p name)) name)
                       (and (stringp preview) (not (string-empty-p preview))
                            (roster--truncate-title preview))
                       roster--untitled)
            :directory directory
            :project-id nil
            :time-updated (if (numberp updated-at)
                              (* roster--ms-per-second updated-at)
                            0)
            :time-archived (when archived 1)
            :tool 'codex))))

(defun roster--codex-list-page (archived &optional cursor)
  "Return one Codex thread page for ARCHIVED state and optional CURSOR."
  (roster--codex-app-server-request
   "thread/list"
   (append `(("limit" . 100)
             ("archived" . ,(if archived t :json-false)))
           (when cursor `(("cursor" . ,cursor))))))

(defun roster--codex-load-state (archived)
  "Return all Codex sessions whose archived state is ARCHIVED."
  (let (sessions cursor)
    (while
        (let* ((page (roster--codex-list-page archived cursor))
               (threads (plist-get page :data)))
          (dolist (thread threads)
            (when-let ((session (roster--codex-thread-from-api thread archived)))
              (push session sessions)))
          (setq cursor (plist-get page :nextCursor))))
    (nreverse sessions)))

(defun roster--codex-load-sessions (&optional include-archived)
  "Return Codex sessions through app-server.
Include archived threads only when INCLUDE-ARCHIVED is non-nil."
  (roster--codex-call-with-app-server
   (lambda ()
     (let ((roster--codex-legacy-titles (roster--codex-load-legacy-titles)))
       (append (roster--codex-load-state nil)
               (when include-archived
                 (roster--codex-load-state t)))))))

;;; Mutations

(defun roster--codex-delete-session (session)
  "Delete Codex SESSION through app-server."
  (let ((session-id (plist-get session :id)))
    (condition-case err
        (roster--codex-app-server-request
         "thread/delete" `(("threadId" . ,session-id)))
      (user-error
       (if (string-match-p "already has an active writer"
                           (error-message-string err))
           (user-error
            "Codex session %s is still open; close its Codex client, then retry"
            session-id)
         (signal (car err) (cdr err)))))
    (roster--codex-delete-legacy-sidecar session-id)))

(defun roster--codex-rename-session (session new-title)
  "Rename Codex SESSION to NEW-TITLE through app-server."
  (let ((session-id (roster--session-id session)))
    (roster--codex-app-server-request
     "thread/name/set"
     `(("threadId" . ,session-id) ("name" . ,new-title)))
    (roster--codex-delete-legacy-sidecar session-id)))

(defun roster--codex-do-archive (session archived)
  "Set Codex SESSION archived state to ARCHIVED through app-server."
  (roster--codex-app-server-request
   (if archived "thread/archive" "thread/unarchive")
   `(("threadId" . ,(plist-get session :id)))))

(defun roster--codex-resume-command (session)
  "Return the shell command used to resume Codex SESSION."
  (format "%s resume %s" roster-codex-command
          (shell-quote-argument (roster--session-id session))))

(defun roster--codex-new-command ()
  "Return the shell command used to start a Codex session."
  roster-codex-command)

(roster-register-backend
 (roster-backend-create
  :id 'codex
  :label "CX"
  :face 'roster-tool-codex-face
  :load #'roster--codex-load-sessions
  :resume-command #'roster--codex-resume-command
  :new-command #'roster--codex-new-command
  :rename #'roster--codex-rename-session
  :archive #'roster--codex-do-archive
  :delete #'roster--codex-delete-session
  :batch #'roster--codex-call-with-app-server))

(provide 'roster-codex)

;;; roster-codex.el ends here
