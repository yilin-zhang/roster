;;; roster-claude.el --- Claude Code backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-core)

;;; Claude Code backend

(defface roster-tool-claude-face
  '((t :inherit font-lock-type-face))
  "Face for the Claude Code tool tag in `roster' lists."
  :group 'roster)

(defcustom roster-claude-dir
  (expand-file-name "~/.claude")
  "Path to the Claude Code configuration directory."
  :type 'directory
  :group 'roster)

(defcustom roster-claude-command "claude"
  "Claude Code executable name or full path."
  :type 'string
  :group 'roster)

(defcustom roster-claude-python-command "python3"
  "Python executable used for the optional Claude Agent SDK bridge."
  :type 'string
  :group 'roster)

(defcustom roster-claude-use-agent-sdk 'auto
  "Whether to use Claude Agent SDK's native session management API.
When `auto', use it when `claude_agent_sdk' is installed and otherwise use
the documented transcript compatibility implementation.  Non-nil requires
the SDK and nil always uses the compatibility implementation."
  :type '(choice (const :tag "Use when available" auto)
                 (const :tag "Require SDK" t)
                 (const :tag "Use transcript compatibility" nil))
  :group 'roster)

(defconst roster--claude-sdk-script
  (expand-file-name "scripts/roster-claude-sdk.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to roster's Claude Agent SDK bridge.")

(defvar roster--claude-sdk-unavailable-key nil
  "SDK configuration known to be unavailable in `auto' mode.")

(defun roster--claude-sdk-cache-key ()
  "Return the configuration key used for SDK availability caching."
  (list roster-claude-python-command roster--claude-sdk-script))

(defun roster--claude-reset-sdk-cache ()
  "Forget a cached negative Claude Agent SDK availability result."
  (interactive)
  (setq roster--claude-sdk-unavailable-key nil)
  (when (called-interactively-p 'interactive)
    (message "Reset roster's Claude Agent SDK availability cache")))

(add-hook 'roster-clear-caches-hook #'roster--claude-reset-sdk-cache)

(defun roster--claude-sdk-call (&rest arguments)
  "Call the Claude Agent SDK bridge with ARGUMENTS and return its result."
  (let ((cache-key (roster--claude-sdk-cache-key)))
    (unless (and (eq roster-claude-use-agent-sdk 'auto)
                 (equal roster--claude-sdk-unavailable-key cache-key))
      (condition-case err
          (with-temp-buffer
            (let ((process-environment
                   (cons (concat "CLAUDE_CONFIG_DIR=" roster-claude-dir)
                         process-environment))
                  (status (apply #'call-process roster-claude-python-command nil t nil
                                 roster--claude-sdk-script arguments)))
              (let ((result (roster--read-json (string-trim (buffer-string)))))
                (cond
                 ((and (plist-member result :available)
                       (not (plist-get result :available)))
                  (when (eq roster-claude-use-agent-sdk 'auto)
                    (setq roster--claude-sdk-unavailable-key cache-key))
                  (when (eq roster-claude-use-agent-sdk t)
                    (user-error "claude-agent-sdk is not installed"))
                  nil)
                 ((not (eq status 0))
                  (user-error "Claude Agent SDK error: %s"
                              (or (plist-get result :error) "unknown error")))
                 (t
                  (setq roster--claude-sdk-unavailable-key nil)
                  result)))))
        (file-missing
         (when (eq roster-claude-use-agent-sdk 'auto)
           (setq roster--claude-sdk-unavailable-key cache-key))
         (when (eq roster-claude-use-agent-sdk t)
           (user-error "Cannot run Claude Agent SDK bridge: %s"
                       (error-message-string err)))
         nil)))))

(defconst roster--claude-cli-entrypoint "cli"
  "Value of a Claude transcript's `entrypoint' field for interactive sessions.
Sessions started through the Claude Agent SDK instead of the CLI record
another value (e.g. \"sdk-py\").  Those are spawned by tooling rather than
by the user, and cannot be resumed with `claude -r', so roster hides them.")

(defconst roster--claude-entrypoint-read-size 65536
  "Maximum number of transcript bytes read when checking its entrypoint.")

(defvar roster--claude-transcript-cache (make-hash-table :test #'equal)
  "Transcript metadata cached by absolute file name.")

(defun roster--claude-clear-transcript-cache ()
  "Discard all cached Claude transcript metadata."
  (clrhash roster--claude-transcript-cache))

(add-hook 'roster-clear-caches-hook #'roster--claude-clear-transcript-cache)

(defun roster--claude-transcript-fingerprint (attributes)
  "Return the cache fingerprint represented by file ATTRIBUTES."
  (when attributes
    (list (file-attribute-modification-time attributes)
          (file-attribute-size attributes))))

(defun roster--claude-prune-transcript-cache (paths)
  "Remove cached transcript entries whose file is not present in PATHS."
  (let ((live (make-hash-table :test #'equal))
        stale)
    (dolist (path paths)
      (puthash path t live))
    (maphash (lambda (path _entry)
               (unless (gethash path live)
                 (push path stale)))
             roster--claude-transcript-cache)
    (dolist (path stale)
      (remhash path roster--claude-transcript-cache))))

(defun roster--claude-jsonl-files (projects-dir)
  "Return `(ENCODED-DIR . PATH)' pairs for Claude JSONL files in PROJECTS-DIR.
Claude Code stores each project under a URL-encoded form of its absolute path
\(e.g. \"-Users-alice-myproject\"); ENCODED-DIR is that raw directory name,
used later as the key for sidecar files."
  ;; Compatibility fallback: Claude Agent SDK is a separate optional install,
  ;; so roster retains support for Claude's documented transcript location.
  ;; Remove this parser only if the SDK ships with the Claude Code CLI itself.
  ;;
  ;; This listing is deliberately non-recursive.  Transcripts of Task-tool
  ;; subagents live one level deeper, in "<encoded-dir>/<session-id>/subagents/",
  ;; and are not sessions the user can resume or rename.  Never turn this into
  ;; `directory-files-recursively' -- that would list every subagent as a
  ;; top-level session.
  (let (result)
    (dolist (encoded-dir (directory-files projects-dir nil "^[^.]"))
      (let ((dir-path (expand-file-name encoded-dir projects-dir)))
        (when (file-directory-p dir-path)
          (dolist (fname (directory-files dir-path nil "\\.jsonl\\'"))
            (push (cons encoded-dir (expand-file-name fname dir-path)) result)))))
    (nreverse result)))


(defun roster--claude-projects-dir ()
  "Return the Claude Code projects directory."
  (expand-file-name "projects" roster-claude-dir))

(defun roster--claude-append-custom-title (encoded-dir session-id title)
  "Append a custom-title record to the Claude Code JSONL for SESSION-ID.
ENCODED-DIR is the Claude-encoded project directory name and TITLE is the
new custom title.  This is equivalent to what Claude Code's /rename
command does internally."
  (let ((path (expand-file-name (concat session-id ".jsonl")
                                (expand-file-name encoded-dir (roster--claude-projects-dir))))
        (record (json-encode `(("type" . "custom-title")
                               ("customTitle" . ,title)
                               ("sessionId" . ,session-id)))))
    (unless (file-exists-p path)
      (error "JSONL file not found: %s" path))
    (write-region (concat record "\n") nil path t 'silent)))

(defun roster--claude-roster-dir ()
  "Return the directory used to store Claude Code roster sidecar files."
  (expand-file-name "roster" roster-claude-dir))

(defun roster--claude-sidecar-path (session-id)
  "Return the path to the roster sidecar JSON file for SESSION-ID."
  (expand-file-name (concat session-id ".roster.json")
                    (roster--claude-roster-dir)))

(defun roster--claude-read-sidecar (session-id)
  "Return roster metadata alist for SESSION-ID, or nil if no sidecar."
  (roster--read-sidecar (roster--claude-sidecar-path session-id)))

(defun roster--claude-write-sidecar (session-id title time-archived)
  "Write roster sidecar JSON for Claude Code SESSION-ID.
TITLE and TIME-ARCHIVED are stored as sidecar fields; either may be nil."
  (roster--write-sidecar (roster--claude-sidecar-path session-id) title time-archived))

(defun roster--claude-clear-legacy-title (session-id)
  "Remove deprecated roster title metadata for Claude SESSION-ID.
Preserve roster's archive state, which Claude has no native equivalent for."
  (let* ((sidecar (roster--claude-read-sidecar session-id))
         (time-archived (cdr (assoc "time_archived" sidecar))))
    (when sidecar
      (roster--claude-write-sidecar session-id nil time-archived))))

(defun roster--claude-update-meta-from-object (obj slug cwd title-candidate
                                                   entrypoint)
  "Update Claude metadata from OBJ.
Return a plist with keys :slug, :cwd, :title-candidate, and :entrypoint.
Called once per JSONL line; treats the accumulator values (SLUG, CWD,
TITLE-CANDIDATE, ENTRYPOINT) as immutable — returns updated copies without
mutation, so the caller can pattern-match the result and rebind all four at
once."
  (let ((new-slug (or slug (plist-get obj :slug)))
        (new-cwd cwd)
        (new-title title-candidate)
        (new-entrypoint entrypoint))
    ;; `entrypoint' is stamped on every message-bearing record, not just on
    ;; user turns, so read it before narrowing to "user".
    (unless new-entrypoint
      (let ((value (plist-get obj :entrypoint)))
        (when (and (stringp value) (not (string-empty-p value)))
          (setq new-entrypoint value))))
    (when (equal (plist-get obj :type) "user")
      (unless new-cwd
        (let ((value (plist-get obj :cwd)))
          (when (and (stringp value) (not (string-empty-p value)))
            (setq new-cwd value))))
      (unless new-title
        (let ((text (roster--content-text
                     (plist-get (plist-get obj :message) :content))))
          ;; Skip system-injected turns such as /compact output, which Claude
          ;; Code stores as user messages beginning with an XML-like tag (e.g.
          ;; "<local-command-caveat>...").  Those are not real session starters.
          (unless (and text (string-prefix-p "<" text))
            (setq new-title text)))))
    (list :slug new-slug
          :cwd new-cwd
          :title-candidate new-title
          :entrypoint new-entrypoint)))

(defun roster--claude-title (meta sidecar)
  "Return display title derived from META and SIDECAR."
  (let ((slug (plist-get meta :slug))
        (candidate (plist-get meta :title-candidate))
        (custom-title (plist-get meta :custom-title))
        (ai-title (plist-get meta :ai-title))
        (sidecar-title (cdr (assoc "title" sidecar))))
    (or sidecar-title
        custom-title
        ai-title
        slug
        (when candidate
          (roster--truncate-title candidate))
        roster--untitled)))

(defun roster--claude-cli-session-p (meta)
  "Return non-nil when META describes an interactive Claude CLI session.
Transcripts written by the Claude Agent SDK -- headless runs spawned by
tooling, such as a `/security-review' subagent -- land beside real sessions
in the projects directory but are not resumable, so roster hides them.
Transcripts predating the `entrypoint' field record nothing and are kept."
  (let ((entrypoint (plist-get meta :entrypoint)))
    (or (null entrypoint)
        (equal entrypoint roster--claude-cli-entrypoint))))

(defun roster--claude-transcript-entrypoint (path)
  "Return the first non-empty `entrypoint' recorded near the start of PATH.
Only a bounded prefix is read so filtering SDK results does not require
loading large transcripts in full.  Return nil on missing, malformed, or
legacy transcripts; callers deliberately keep those sessions visible."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path nil 0 roster--claude-entrypoint-read-size)
        (goto-char (point-min))
        (catch 'entrypoint
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (point) (line-end-position)))
                   (obj (roster--read-json line))
                   (entrypoint (plist-get obj :entrypoint)))
              (when (and (stringp entrypoint)
                         (not (string-empty-p entrypoint)))
                (throw 'entrypoint entrypoint)))
            (forward-line 1))
          nil))
    (error nil)))

(defun roster--claude-session-from-file (encoded-dir path)
  "Return unified Claude session plist for ENCODED-DIR and JSONL file at PATH.
Return nil when PATH is not an interactive CLI session transcript."
  (let* ((session-id (file-name-sans-extension (file-name-nondirectory path)))
         (meta (roster--claude-parse-jsonl path)))
    (when (and meta (roster--claude-cli-session-p meta))
      (let* ((sidecar (roster--claude-read-sidecar session-id))
             (cwd (plist-get meta :cwd))
             (time-archived (let ((value (cdr (assoc "time_archived" sidecar))))
                              (when (numberp value) value))))
        (list :id session-id
              :title (roster--claude-title meta sidecar)
              :directory (expand-file-name (if (string-empty-p cwd) "~" cwd))
              :project-id encoded-dir
              :time-updated (plist-get meta :time-updated)
              :time-archived time-archived
              :tool 'claude
              :encoded-dir encoded-dir
              :file-path path)))))

(defun roster--claude-parse-jsonl-uncached (path attributes)
  "Return metadata plist from a Claude Code JSONL file at PATH.
ATTRIBUTES are the file attributes captured before reading.
Returns plist with keys :slug, :cwd, :title-candidate, :entrypoint,
:custom-title, :ai-title, and :time-updated (file mtime in milliseconds)."
  (condition-case nil
      (let (slug cwd title-candidate entrypoint custom-title ai-title)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                   (obj (roster--read-json line)))
              (when obj
                (pcase-let ((`(:slug ,new-slug :cwd ,new-cwd :title-candidate ,new-title
                                     :entrypoint ,new-entrypoint)
                             (roster--claude-update-meta-from-object
                              obj slug cwd title-candidate entrypoint)))
                  (setq slug new-slug
                        cwd new-cwd
                        title-candidate new-title
                        entrypoint new-entrypoint))
                (pcase (plist-get obj :type)
                  ("custom-title"
                   (let ((value (plist-get obj :customTitle)))
                     (when (and (stringp value) (not (string-empty-p value)))
                       (setq custom-title value))))
                  ("ai-title"
                   (let ((value (plist-get obj :aiTitle)))
                     (when (and (stringp value) (not (string-empty-p value)))
                       (setq ai-title value)))))))
            (forward-line 1)))
        (let* ((mtime (when attributes
                        (file-attribute-modification-time attributes)))
               (time-updated (if mtime
                                 (floor (* roster--ms-per-second (float-time mtime)))
                               0)))
          (list :slug slug
                :cwd (or cwd "")
                :title-candidate title-candidate
                :entrypoint entrypoint
                :custom-title custom-title
                :ai-title ai-title
                :time-updated time-updated)))
    (error nil)))

(defun roster--claude-parse-jsonl (path)
  "Return cached metadata parsed from Claude Code JSONL file PATH."
  (let* ((attributes (file-attributes path))
         (fingerprint (roster--claude-transcript-fingerprint attributes))
         (cached (gethash path roster--claude-transcript-cache)))
    (if (and fingerprint (equal (car cached) fingerprint))
        (cdr cached)
      (let ((metadata (roster--claude-parse-jsonl-uncached path attributes)))
        (when (and metadata
                   (equal fingerprint
                          (roster--claude-transcript-fingerprint
                           (file-attributes path))))
          (puthash path (cons fingerprint metadata)
                   roster--claude-transcript-cache))
        metadata))))

(defun roster--claude-load-sessions-from-transcripts (&optional include-archived)
  "Return Claude sessions from documented transcripts.
INCLUDE-ARCHIVED controls roster's sidecar archive metadata."
  (let ((projects-dir (roster--claude-projects-dir)))
    (when (file-directory-p projects-dir)
      (let* ((files (roster--claude-jsonl-files projects-dir))
             (paths (mapcar #'cdr files))
             (sessions
              (delq nil
                    (mapcar (lambda (entry)
                              (roster--claude-session-from-file
                               (car entry) (cdr entry)))
                            files))))
        (roster--claude-prune-transcript-cache paths)
        (if include-archived sessions (roster--active-sessions sessions))))))

(defun roster--claude-session-from-sdk (value)
  "Return unified Claude session parsed from SDK bridge VALUE."
  (let* ((session-id (plist-get value :id))
         (sidecar (roster--claude-read-sidecar session-id))
         (time-archived (cdr (assoc "time_archived" sidecar))))
    (list :id session-id
          :title (or (cdr (assoc "title" sidecar))
                     (plist-get value :title)
                     roster--untitled)
          :directory (expand-file-name (or (plist-get value :directory) "~"))
          :project-id nil
          :time-updated (or (plist-get value :time_updated) 0)
          :time-archived (and (numberp time-archived) time-archived)
          :tool 'claude)))

(defun roster--claude-sdk-session-resumable-p (value)
  "Return non-nil when SDK session VALUE can be resumed by Claude CLI.
The Agent SDK's `list_sessions' includes all top-level transcripts without
exposing their entrypoint, so consult the corresponding transcript when it
is available.  Missing and legacy transcripts are kept for compatibility."
  (let ((path (roster--claude-find-transcript (plist-get value :id))))
    (or (null path)
        (roster--claude-cli-session-p
         (list :entrypoint (roster--claude-transcript-entrypoint path))))))

(defun roster--claude-load-sessions (&optional include-archived)
  "Return Claude Code sessions, optionally INCLUDE-ARCHIVED sessions."
  (let* ((sdk-result (when roster-claude-use-agent-sdk
                       (roster--claude-sdk-call "list")))
         (sessions
          (if sdk-result
              (delq nil
                    (mapcar (lambda (value)
                              (when (roster--claude-sdk-session-resumable-p value)
                                (roster--claude-session-from-sdk value)))
                            (plist-get sdk-result :sessions)))
            (roster--claude-load-sessions-from-transcripts t))))
    (if include-archived sessions (roster--active-sessions sessions))))

(defun roster--claude-find-transcript (session-id)
  "Return transcript path for Claude SESSION-ID, or nil.
Only project directories are searched, never their nested \"subagents\"
directories: a subagent transcript is not a session and must never be
reachable by a delete or rename that names a session id."
  (let ((projects-dir (roster--claude-projects-dir))
        (fname (concat session-id ".jsonl"))
        found)
    (when (file-directory-p projects-dir)
      (dolist (encoded-dir (directory-files projects-dir nil "^[^.]"))
        (unless found
          (let ((path (expand-file-name
                       fname (expand-file-name encoded-dir projects-dir))))
            (when (file-regular-p path)
              (setq found path))))))
    found))

(defun roster--claude-delete-session (session)
  "Delete a Claude Code SESSION's JSONL file and roster sidecar."
  ;; Claude exposes no per-session delete API.  Its official docs identify the
  ;; JSONL transcript as the persisted session, so trashing it is the narrowest
  ;; native-format-compatible operation.  Revisit when an SDK API is added.
  (let* ((session-id (plist-get session :id))
         (jsonl (or (plist-get session :file-path)
                    (roster--claude-find-transcript session-id)))
         (sidecar (roster--claude-sidecar-path session-id)))
    (when (and jsonl (file-exists-p jsonl))
      (move-file-to-trash jsonl))
    (when (file-exists-p sidecar)
      (delete-file sidecar))))

(defun roster--claude-rename-session (session new-title)
  "Rename Claude Code SESSION to NEW-TITLE."
  (let ((session-id (roster--session-id session)))
    (if (and roster-claude-use-agent-sdk
             (roster--claude-sdk-call
              "rename" session-id new-title
              (roster--session-directory session)))
        t
      ;; Compatibility fallback mirrors the native `/rename' record.  It is
      ;; intentionally isolated for removal once the SDK is a bundled dependency.
      (roster--claude-append-custom-title
       (plist-get session :encoded-dir) session-id new-title))
    (roster--claude-clear-legacy-title session-id)
    t))

(defun roster--claude-do-archive (session archived)
  "Set a Claude Code SESSION archived state to ARCHIVED without prompting."
  ;; Local Claude CLI sessions have no archive API.  Archive is therefore a
  ;; roster-only view preference, kept in a sidecar rather than corrupting the
  ;; SDK's single user-controlled tag field.
  (let* ((session-id (plist-get session :id))
         (sidecar (roster--claude-read-sidecar session-id))
         (new-archived (when archived
                         (floor (* roster--ms-per-second (float-time (current-time)))))))
    (roster--claude-write-sidecar
     session-id (cdr (assoc "title" sidecar)) new-archived)))

(defun roster--claude-resume-command (session)
  "Return the shell command used to resume Claude Code SESSION."
  (format "%s -r %s" roster-claude-command
          (shell-quote-argument (roster--session-id session))))

(defun roster--claude-new-command ()
  "Return the shell command used to start a Claude Code session."
  roster-claude-command)

(roster-register-backend
 (roster-backend-create
  :id 'claude
  :label "CC"
  :face 'roster-tool-claude-face
  :load #'roster--claude-load-sessions
  :resume-command #'roster--claude-resume-command
  :new-command #'roster--claude-new-command
  :rename #'roster--claude-rename-session
  :archive #'roster--claude-do-archive
  :delete #'roster--claude-delete-session))

(provide 'roster-claude)

;;; roster-claude.el ends here
