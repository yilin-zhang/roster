;;; roster-pi.el --- pi backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-core)

;;; pi backend

(defface roster-tool-pi-face
  `((t :foreground ,(face-attribute 'ansi-color-red :foreground)))
  "Face for the pi tool tag in `roster' lists."
  :group 'roster)

(defcustom roster-pi-dir
  (expand-file-name "~/.pi/agent")
  "Path to the pi configuration directory."
  :type 'directory
  :group 'roster)

(defcustom roster-pi-command "pi"
  "The pi executable name or full path."
  :type 'string
  :group 'roster)

(defun roster--pi-sessions-dir ()
  "Return the pi sessions directory."
  (expand-file-name "sessions" roster-pi-dir))

(defun roster--pi-roster-dir ()
  "Return the directory used to store pi roster sidecar files."
  (expand-file-name "roster" roster-pi-dir))

(defun roster--pi-sidecar-path (session-id)
  "Return the path to the roster sidecar JSON file for pi SESSION-ID."
  (expand-file-name (concat session-id ".roster.json")
                    (roster--pi-roster-dir)))

(defun roster--pi-read-sidecar (session-id)
  "Return roster metadata alist for pi SESSION-ID, or nil if no sidecar."
  (roster--read-sidecar (roster--pi-sidecar-path session-id)))

(defun roster--pi-write-sidecar (session-id title time-archived)
  "Write roster sidecar JSON for pi SESSION-ID.
TITLE and TIME-ARCHIVED are stored as sidecar fields; either may be nil."
  (roster--write-sidecar (roster--pi-sidecar-path session-id) title time-archived))

(defun roster--pi-parse-jsonl (path)
  "Return metadata plist from a pi JSONL file at PATH.
Returns plist with keys :id, :cwd, :title-candidate, :session-name,
:last-entry-id, and :time-updated."
  ;; Compatibility implementation of pi's public SessionManager/session-file
  ;; contract.  Keep this until pi provides an installed, stable management
  ;; executable; its SDK is currently a separate JavaScript import surface.
  (condition-case nil
      (let (session-id cwd title-candidate session-name last-entry-id)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (not (eobp))
            (when-let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                        (obj (roster--read-json line))
                        (type (plist-get obj :type)))
              (pcase type
                ("session"
                 (unless session-id
                   (when-let ((value (plist-get obj :id)))
                     (setq session-id value)))
                 (unless cwd
                   (when-let ((value (plist-get obj :cwd)))
                     (setq cwd value))))
                ((or "message" "model_change" "thinking_level_change" "compaction"
                     "branch_summary" "custom" "custom_message" "label" "session_info")
                 (when-let ((value (plist-get obj :id)))
                   (setq last-entry-id value))
                 (when (and (equal type "message") (not title-candidate))
                   (let ((message (plist-get obj :message)))
                     (when (equal (plist-get message :role) "user")
                       (let ((text (roster--content-text (plist-get message :content))))
                         (unless (and text (string-prefix-p "/" text))
                           (setq title-candidate text))))))
                 (when (equal type "session_info")
                   (when-let ((value (plist-get obj :name)))
                     (unless (string-empty-p value)
                       (setq session-name value)))))))
            (forward-line 1)))
        (let* ((attrs (file-attributes path))
               (mtime (when attrs (file-attribute-modification-time attrs)))
               (time-updated (if mtime (floor (* roster--ms-per-second (float-time mtime))) 0)))
          (list :id session-id
                :cwd (or cwd "")
                :title-candidate title-candidate
                :session-name session-name
                :last-entry-id last-entry-id
                :time-updated time-updated)))
    (error nil)))

(defun roster--pi-session-title (meta sidecar)
  "Return display title derived from META and SIDECAR."
  (let ((sidecar-title (cdr (assoc "title" sidecar)))
        (session-name (plist-get meta :session-name))
        (candidate (plist-get meta :title-candidate))
        (cwd (plist-get meta :cwd)))
    (or sidecar-title
        session-name
        (when candidate
          (roster--truncate-title candidate))
        (let ((base (file-name-nondirectory (directory-file-name (expand-file-name (if (string-empty-p cwd) "~" cwd))))))
          (unless (string-empty-p base) base))
        roster--untitled)))

(defun roster--pi-session-from-file (path)
  "Return a unified pi session plist for JSONL file at PATH."
  (when-let* ((meta (roster--pi-parse-jsonl path))
              (session-id (plist-get meta :id)))
    (let* ((sidecar (roster--pi-read-sidecar session-id))
           (cwd (plist-get meta :cwd))
           (dir (expand-file-name (if (string-empty-p cwd) "~" cwd)))
           (time-archived (let ((value (cdr (assoc "time_archived" sidecar))))
                            (when (numberp value) value))))
      (list :id session-id
            :title (roster--pi-session-title meta sidecar)
            :directory dir
            :project-id nil
            :time-updated (plist-get meta :time-updated)
            :time-archived time-archived
            :tool 'pi
            :file-path path
            :last-entry-id (plist-get meta :last-entry-id)))))

(defun roster--pi-load-sessions (&optional include-archived)
  "Return pi sessions, optionally INCLUDE-ARCHIVED sessions."
  (let ((sessions-dir (roster--pi-sessions-dir)))
    (when (file-directory-p sessions-dir)
      (let ((sessions
             (seq-keep #'roster--pi-session-from-file
                       (directory-files-recursively sessions-dir "\\.jsonl\\'"))))
        (if include-archived sessions (roster--active-sessions sessions))))))

(defun roster--pi-entry-id ()
  "Return a fresh pi session entry id."
  (substring (md5 (format "%s-%s-%s" (float-time) (emacs-pid) (random))) 0 8))

(defun roster--pi-append-session-info (session title)
  "Append a pi `session_info' entry naming SESSION as TITLE."
  ;; This is pi's documented native format and the same operation exposed as
  ;; SessionManager.appendSessionInfo()/pi.setSessionName().  Prefer a future
  ;; management CLI or standalone RPC endpoint when one becomes available.
  (let* ((path (plist-get session :file-path))
         (entry `(("type" . "session_info")
                  ("id" . ,(roster--pi-entry-id))
                  ("parentId" . ,(plist-get session :last-entry-id))
                  ("timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" (current-time) t))
                  ("name" . ,title))))
    (write-region (concat (json-encode entry) "\n") nil path t 'silent)))

(defun roster--pi-delete-session (session)
  "Delete a pi SESSION's JSONL file and roster sidecar."
  ;; pi's official session documentation explicitly defines deletion as
  ;; removing the JSONL and recommends trash when available.
  (let* ((session-id (plist-get session :id))
         (file-path (plist-get session :file-path))
         (sidecar (roster--pi-sidecar-path session-id)))
    (when (file-exists-p file-path)
      (move-file-to-trash file-path))
    (when (file-exists-p sidecar)
      (delete-file sidecar))))

(defun roster--pi-rename-session (session new-title)
  "Rename pi SESSION to NEW-TITLE."
  (roster--pi-append-session-info session new-title))

(defun roster--pi-do-archive (session archived)
  "Set a pi SESSION archived state to ARCHIVED without prompting."
  ;; pi has no archive concept.  Keep this roster-only display state separate
  ;; from the native session tree.
  (let* ((session-id (plist-get session :id))
         (sidecar (roster--pi-read-sidecar session-id))
         (sidecar-title (when sidecar (cdr (assoc "title" sidecar)))))
    (roster--pi-write-sidecar
     session-id sidecar-title
     (when archived (floor (* roster--ms-per-second (float-time (current-time))))))))

(defun roster--pi-resume-command (session)
  "Return the shell command used to resume pi SESSION."
  (format "%s --session %s" roster-pi-command
          (shell-quote-argument (plist-get session :file-path))))

(defun roster--pi-new-command ()
  "Return the shell command used to start a pi session."
  roster-pi-command)

(roster-register-backend
 (roster-backend-create
  :id 'pi
  :label "PI"
  :face 'roster-tool-pi-face
  :load #'roster--pi-load-sessions
  :resume-command #'roster--pi-resume-command
  :new-command #'roster--pi-new-command
  :rename #'roster--pi-rename-session
  :archive #'roster--pi-do-archive
  :delete #'roster--pi-delete-session))

(provide 'roster-pi)

;;; roster-pi.el ends here
