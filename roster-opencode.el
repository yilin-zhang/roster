;;; roster-opencode.el --- OpenCode backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; OpenCode backend using the official local server API.

;;; Code:

(require 'roster-core)
(require 'url)
(require 'url-http)
(require 'url-util)

;;; OpenCode backend

(defface roster-tool-opencode-face
  `((t :foreground ,(face-attribute 'ansi-color-blue :foreground)))
  "Face for the OpenCode tool tag in `roster' lists."
  :group 'roster)

(defcustom roster-opencode-command "opencode"
  "OpenCode executable name or full path."
  :type 'string
  :group 'roster)

(defcustom roster-opencode-db-path
  (expand-file-name "~/.local/share/opencode/opencode.db")
  "Path to the OpenCode database used only for legacy unarchive support.
OpenCode's server API currently supports archiving but not clearing an archive
timestamp.  This compatibility option can be removed when it gains a native
unarchive operation."
  :type 'file
  :group 'roster)

(defconst roster--opencode-server-timeout 5
  "Seconds to wait for OpenCode's local server and HTTP responses.")

(defvar roster--opencode-process nil)
(defvar roster--opencode-output nil)
(defvar roster--opencode-base-url nil)

;;; Server lifecycle

(defun roster--opencode-server-url (buffer)
  "Return the OpenCode server URL announced in BUFFER, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "opencode server listening on \\(http://[^[:space:]]+\\)" nil t)
        (match-string-no-properties 1)))))

(defun roster--opencode-wait-for-server (process buffer)
  "Wait for PROCESS to announce its URL in BUFFER and return that URL."
  (let ((deadline (+ (float-time) roster--opencode-server-timeout))
        url)
    (while (and (not url)
                (process-live-p process)
                (< (float-time) deadline))
      (accept-process-output process 0.05)
      (setq url (roster--opencode-server-url buffer)))
    (or url
        (user-error "OpenCode server failed to start: %s"
                    (string-trim
                     (with-current-buffer buffer (buffer-string)))))))

(defun roster--opencode-call-with-server (function)
  "Call FUNCTION while one scoped OpenCode server is available."
  (let ((output (generate-new-buffer " *roster-opencode-server*"))
        process)
    (unwind-protect
        (progn
          ;; This is a fresh loopback-only server on an ephemeral port.  Do not
          ;; inherit credentials intended for a user's persistent server.
          (let ((process-environment
                 (seq-remove
                  (lambda (entry)
                    (string-match-p
                     "\\`OPENCODE_SERVER_\\(?:PASSWORD\\|USERNAME\\)=" entry))
                  process-environment)))
            (setq process
                  (make-process
                   :name "roster-opencode-server"
                   :buffer output
                   :stderr output
                   :command (list roster-opencode-command "serve" "--pure"
                                  "--hostname" "127.0.0.1" "--port" "0")
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t)))
          (let ((roster--opencode-process process)
                (roster--opencode-output output)
                (roster--opencode-base-url
                 (roster--opencode-wait-for-server process output)))
            (funcall function)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer output))))

(defun roster--opencode-call-with-server-if-needed (function)
  "Call FUNCTION, reusing the dynamically scoped OpenCode server."
  (if (and roster--opencode-process
           (process-live-p roster--opencode-process)
           roster--opencode-base-url)
      (funcall function)
    (roster--opencode-call-with-server function)))

;;; HTTP protocol

(defun roster--opencode-query-string (parameters)
  "Return URL query string for non-nil PARAMETERS."
  (when-let ((values
              (seq-keep
               (lambda (pair)
                 (when (cdr pair)
                   (format "%s=%s" (car pair)
                           (url-hexify-string (format "%s" (cdr pair))))))
               parameters)))
    (concat "?" (string-join values "&"))))

(defun roster--opencode-response (buffer)
  "Decode an OpenCode HTTP response in BUFFER.
Return a cons of the response headers and decoded JSON body."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((status (if (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                      (string-to-number (match-string 1))
                    0))
          headers)
      (while (re-search-forward
              "^\\([^:\r\n]+\\):[[:space:]]*\\([^\r\n]*\\)\r?$"
              (save-excursion
                (re-search-forward "\r?\n\r?\n" nil 'move)
                (point))
              t)
        (push (cons (downcase (match-string-no-properties 1))
                    (match-string-no-properties 2))
              headers))
      (re-search-forward "\r?\n\r?\n" nil 'move)
      (let* ((raw-body (unless (eobp)
                         (buffer-substring-no-properties (point) (point-max))))
             ;; `url-retrieve-synchronously' leaves JSON response bodies as
             ;; unibyte strings.  Decode UTF-8 explicitly so non-ASCII titles
             ;; are not rendered as replacement characters.
             (body (when raw-body
                     (roster--read-json
                      (if (multibyte-string-p raw-body)
                          raw-body
                        (decode-coding-string raw-body 'utf-8 t))))))
        (unless (<= 200 status 299)
          (user-error "OpenCode API returned HTTP %s: %s" status body))
        (cons headers body)))))

(defun roster--opencode-request (method path &optional parameters body)
  "Send METHOD to OpenCode PATH with PARAMETERS and JSON BODY.
Return a cons of response headers and decoded JSON body."
  (roster--opencode-call-with-server-if-needed
   (lambda ()
     (let* ((url-request-method method)
            (url-request-extra-headers
             (when body '(("Content-Type" . "application/json"))))
            (url-request-data (when body (encode-coding-string
                                          (json-encode body) 'utf-8)))
            (url-proxy-services nil)
            (url (concat roster--opencode-base-url path
                         (roster--opencode-query-string parameters)))
            (buffer (url-retrieve-synchronously
                     url t nil roster--opencode-server-timeout)))
       (unless buffer
         (user-error "OpenCode API did not respond: %s %s" method path))
       (unwind-protect
           (roster--opencode-response buffer)
         (kill-buffer buffer))))))

(defun roster--opencode-api-body (method path &optional parameters body)
  "Return decoded OpenCode response body for METHOD and PATH."
  (cdr (roster--opencode-request method path parameters body)))

;;; Session loading

(defun roster--opencode-session-from-api (value)
  "Return unified session parsed from OpenCode API VALUE."
  (let ((time (plist-get value :time)))
    (list :id (plist-get value :id)
          :title (or (plist-get value :title) roster--untitled)
          :directory (expand-file-name
                      (or (plist-get value :directory) "~"))
          :project-id (plist-get value :projectID)
          :time-updated (or (plist-get time :updated) 0)
          :time-archived (plist-get time :archived)
          :tool 'opencode)))

(defun roster--opencode-list-page (include-archived &optional cursor)
  "Return one root-session page and next cursor for INCLUDE-ARCHIVED and CURSOR."
  (let* ((response
          (roster--opencode-request
           "GET" "/experimental/session"
           `(("roots" . "true")
             ("archived" . ,(when include-archived "true"))
             ("limit" . "100")
             ("cursor" . ,cursor))))
         (headers (car response)))
    (cons (cdr response) (cdr (assoc "x-next-cursor" headers)))))

(defun roster--opencode-load-sessions (&optional include-archived)
  "Return root OpenCode sessions through the official local server API.
INCLUDE-ARCHIVED controls whether archived sessions are included."
  (roster--opencode-call-with-server
   (lambda ()
     (let (sessions cursor)
       (while
           (let ((page (roster--opencode-list-page include-archived cursor)))
             (setq sessions
                   (nconc sessions
                          (mapcar #'roster--opencode-session-from-api (car page)))
                   cursor (cdr page))))
       sessions))))

;;; Mutations

(defun roster--opencode-session-path (session)
  "Return native API path for OpenCode SESSION."
  (format "/session/%s"
          (url-hexify-string (roster--session-id session))))

(defun roster--opencode-session-directory-query (session)
  "Return native API directory query for OpenCode SESSION."
  `(("directory" . ,(roster--session-directory session))))

(defun roster--opencode-update-session (session body)
  "Update OpenCode SESSION with BODY through the native API."
  (roster--opencode-api-body
   "PATCH" (roster--opencode-session-path session)
   (roster--opencode-session-directory-query session) body))

(defun roster--opencode-delete-session (session)
  "Delete OpenCode SESSION through the native API."
  (roster--opencode-api-body
   "DELETE" (roster--opencode-session-path session)
   (roster--opencode-session-directory-query session)))

(defun roster--opencode-rename-session (session new-title)
  "Rename OpenCode SESSION to NEW-TITLE through the native API."
  (let ((updated (roster--opencode-update-session
                  session `(("title" . ,new-title)))))
    (unless (equal (plist-get updated :title) new-title)
      (user-error "Session %s failed title verification"
                  (roster--session-id session)))))

(defun roster--opencode-unarchive-compat (session)
  "Unarchive OpenCode SESSION through its deprecated SQLite compatibility path.
OpenCode 1.18's native PATCH endpoint ignores a missing archive time and rejects
JSON null, so it cannot express `Session.setArchived(..., undefined)'.  Keep
this small fallback isolated until the server exposes native unarchive."
  (unless (and (require 'sqlite nil t) (sqlite-available-p))
    (user-error "This OpenCode version has no native unarchive API"))
  (unless (file-readable-p roster-opencode-db-path)
    (user-error "OpenCode database not found: %s" roster-opencode-db-path))
  (let ((db (sqlite-open roster-opencode-db-path)))
    (unwind-protect
        (unless (= 1 (sqlite-execute
                      db "UPDATE session SET time_archived = NULL WHERE id = ?"
                      (vector (roster--session-id session))))
          (user-error "No session updated for id %s"
                      (roster--session-id session)))
      (sqlite-close db))))

(defun roster--opencode-archive-session (session archived)
  "Set OpenCode SESSION archive state to ARCHIVED."
  (if archived
      (let* ((timestamp (floor (* roster--ms-per-second
                                  (float-time (current-time)))))
             (updated
              (roster--opencode-update-session
               session `(("time" . (("archived" . ,timestamp)))))))
        (unless (numberp (plist-get (plist-get updated :time) :archived))
          (user-error "Session %s failed archive verification"
                      (roster--session-id session))))
    (roster--opencode-unarchive-compat session)))

(defun roster--opencode-move-session (session)
  "Interactively move OpenCode SESSION through its native control-plane API."
  (let* ((old-dir (roster--session-directory session))
         (new-dir (directory-file-name
                   (expand-file-name
                    (read-directory-name
                     (format "New directory (current: %s): " old-dir)
                     old-dir nil t)))))
    (cond
     ((string= old-dir new-dir)
      (message "Session %s already points to %s"
               (roster--session-id session) new-dir)
      nil)
     ((not (yes-or-no-p
            (format "Move session %s from %s to %s? "
                    (roster--session-id session) old-dir new-dir)))
      nil)
     (t
      (roster--opencode-api-body
       "POST" "/experimental/control-plane/move-session" nil
       `(("sessionID" . ,(roster--session-id session))
         ("destination" . (("directory" . ,new-dir)))
         ("moveChanges" . :json-false)))
      (message "Moved session %s to %s" (roster--session-id session) new-dir)
      t))))

(defun roster--opencode-resume-command (session)
  "Return the shell command used to resume OpenCode SESSION."
  (format "%s -s %s" roster-opencode-command
          (shell-quote-argument (roster--session-id session))))

(defun roster--opencode-new-command ()
  "Return the shell command used to start an OpenCode session."
  roster-opencode-command)

(roster-register-backend
 (roster-backend-create
  :id 'opencode
  :label "OC"
  :face 'roster-tool-opencode-face
  :load #'roster--opencode-load-sessions
  :resume-command #'roster--opencode-resume-command
  :new-command #'roster--opencode-new-command
  :rename #'roster--opencode-rename-session
  :archive #'roster--opencode-archive-session
  :delete #'roster--opencode-delete-session
  :move #'roster--opencode-move-session
  :batch #'roster--opencode-call-with-server))

(provide 'roster-opencode)

;;; roster-opencode.el ends here
