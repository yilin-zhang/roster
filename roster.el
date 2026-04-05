;;; roster.el --- Session manager for coding agents -*- lexical-binding: t; -*-

;; Author: Yilin Zhang
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience

;;; Commentary:

;; Roster manages AI coding sessions for OpenCode, Claude Code, and Codex inside Emacs.
;;
;; Sessions from all tools are shown in a unified `tabulated-list-mode' buffer
;; tagged "OC" (OpenCode), "CC" (Claude Code), or "CX" (Codex).  Supported operations:
;;   resume, rename, archive/unarchive, delete, and directory moves (OpenCode only).
;;
;; Storage backends:
;;   OpenCode  — reads and writes the SQLite database at `roster-opencode-db-path'.
;;   Claude Code — reads JSONL conversation files under `roster-claude-dir'/projects/.
;;                 Custom metadata (title, archive state) is kept in .roster.json
;;                 sidecar files because Claude Code's database is not third-party
;;                 writable.
;;   Codex     — reads JSONL files under `roster-codex-dir'/sessions/ (active) and
;;                 archived_sessions/ (archived).  Custom metadata is kept in
;;                 .roster.json sidecar files under `roster-codex-dir'/roster/.
;;
;; Requires Emacs 29.1+ for built-in SQLite support (sqlite.el).

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)
(require 'tabulated-list)
(require 'json)
(require 'ansi-color)


(defgroup roster nil
  "Session manager for OpenCode and Claude Code."
  :group 'tools
  :prefix "roster-")

;;; Faces

(defface roster-list-title-face
  '((t :inherit default :weight bold))
  "Face for session titles in `roster' lists."
  :group 'roster)

(defface roster-list-active-face
  '((t :inherit success))
  "Face for active session state in `roster' lists."
  :group 'roster)

(defface roster-list-archived-face
  '((t :inherit shadow :slant italic))
  "Face for archived session state in `roster' lists."
  :group 'roster)

(defface roster-list-project-face
  '((t :inherit font-lock-builtin-face))
  "Face for project names in `roster' lists."
  :group 'roster)

(defface roster-list-directory-face
  '((t :inherit font-lock-comment-face))
  "Face for directory paths in `roster' lists."
  :group 'roster)

(defface roster-list-time-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for timestamps in `roster' lists."
  :group 'roster)

(defface roster-list-tool-opencode-face
  `((t :foreground ,(face-attribute 'ansi-color-blue :foreground)))
  "Face for the OpenCode tool tag in `roster' lists."
  :group 'roster)

(defface roster-list-tool-claude-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
  "Face for the Claude Code tool tag in `roster' lists."
  :group 'roster)

(defface roster-list-tool-codex-face
  `((t :foreground ,(face-attribute 'ansi-color-green :foreground)))
  "Face for the Codex tool tag in `roster' lists."
  :group 'roster)

(defface roster-list-mark-face
  '((((background dark)) (:background "DarkGoldenrod4"))
    (t (:background "LightYellow1")))
  "Face for marked rows in `roster' lists."
  :group 'roster)

(defface roster-list-mark-indicator-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
  "Face for the mark indicator character in `roster' lists."
  :group 'roster)

;;; Customization

(defcustom roster-opencode-db-path
  (expand-file-name "~/.local/share/opencode/opencode.db")
  "Path to OpenCode SQLite database."
  :type 'file
  :group 'roster)

(defcustom roster-opencode-command "opencode"
  "OpenCode executable name or full path."
  :type 'string
  :group 'roster)

(defcustom roster-terminal-function #'roster-open-in-ghostty
  "Function used to open an external terminal and run a command.
The function is called with two args: DIRECTORY and COMMAND."
  :type 'function
  :group 'roster)

(defcustom roster-list-include-archived t
  "Whether `roster-list-sessions' shows archived sessions by default."
  :type 'boolean
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

(defcustom roster-codex-dir
  (expand-file-name "~/.codex")
  "Path to the Codex configuration directory."
  :type 'directory
  :group 'roster)

(defcustom roster-codex-command "codex"
  "Codex executable name or full path."
  :type 'string
  :group 'roster)

(defcustom roster-enabled-tools '(opencode claude codex)
  "List of tools whose sessions are shown by roster.
Valid elements are the symbols `opencode', `claude', and `codex'."
  :type '(set (const opencode) (const claude) (const codex))
  :group 'roster)

(defcustom roster-default-new-session-tool 'opencode
  "Default tool when creating a new session and multiple tools are enabled.
Must be a symbol present in `roster-enabled-tools'."
  :type '(choice (const opencode) (const claude) (const codex))
  :group 'roster)

;;; Variables

(defvar roster--completion-directory-map nil
  "Alist from displayed candidate title to session directory.")

(defvar roster-list-buffer-name "*roster*"
  "Buffer name used for the main `roster' session list.")

(defvar-local roster-list-source-function nil
  "Function returning sessions for the current `roster' list buffer.")

(defvar-local roster-list-show-archived roster-list-include-archived
  "Whether the current `roster' list buffer shows archived sessions.")

(defvar-local roster-list--marked (make-hash-table :test #'equal)
  "Hash table of marked session IDs in the current `roster' list buffer.")

(defvar-local roster-list--mark-overlays (make-hash-table :test #'equal)
  "Hash table mapping session ID to its mark overlay in the current buffer.")

;;; Constants

(defconst roster--ms-per-second 1000
  "Conversion factor between seconds and milliseconds for session timestamps.")

(defconst roster--title-max-length 60
  "Maximum character length for auto-derived session titles before truncation.")

(defconst roster--untitled "(untitled)"
  "Fallback display title for sessions with no identifiable title.")

;;; Core

(defun roster--session-tool (session)
  "Return SESSION backend symbol, defaulting to `opencode'."
  (or (plist-get session :tool) 'opencode))

(defun roster--session-id (session)
  "Return SESSION id."
  (plist-get session :id))

(defun roster--session-title (session)
  "Return SESSION title."
  (plist-get session :title))

(defun roster--session-directory (session)
  "Return SESSION directory."
  (plist-get session :directory))

(defun roster--sort-sessions (sessions)
  "Return SESSIONS sorted by descending update time."
  (sort sessions
        (lambda (a b)
          (> (or (plist-get a :time-updated) 0)
             (or (plist-get b :time-updated) 0)))))

(defun roster--select-from-sessions (sessions prompt missing-message)
  "Select one entry from SESSIONS using PROMPT.
Signal MISSING-MESSAGE when SESSIONS is empty."
  (unless sessions
    (user-error "%s" missing-message))
  (roster--select-session sessions prompt))

(defun roster--session-archived-p (session)
  "Return non-nil when SESSION is archived."
  (numberp (plist-get session :time-archived)))

(defun roster--active-sessions (sessions)
  "Return unarchived SESSIONS."
  (seq-remove #'roster--session-archived-p sessions))

(defun roster--archived-sessions (sessions)
  "Return archived SESSIONS."
  (seq-filter #'roster--session-archived-p sessions))

(defun roster--session-display-title (session)
  "Return display title for SESSION."
  (if (roster--session-archived-p session)
      (format "%s [archived]" (plist-get session :title))
    (plist-get session :title)))

(defun roster--session-state (session)
  "Return display state for SESSION."
  (if (roster--session-archived-p session)
      "archived"
    "active"))

(defun roster--format-time-millis (millis)
  "Format MILLIS since epoch for list display."
  (if (and millis (> millis 0))
      (format-time-string "%Y-%m-%d %H:%M"
                          (seconds-to-time (/ millis (float roster--ms-per-second))))
    ""))

(defun roster--state-face (session)
  "Return the face used for SESSION state."
  (if (roster--session-archived-p session)
      'roster-list-archived-face
    'roster-list-active-face))

(defun roster--session-by-id (session-id)
  "Return root session plist for SESSION-ID, or nil when missing."
  (when session-id
    (seq-find (lambda (session)
                (string= (plist-get session :id) session-id))
              (roster--load-sessions))))

(defun roster--run-command (directory command)
  "Run COMMAND in DIRECTORY and return its trimmed stdout.
Signal a `user-error' when the command exits unsuccessfully."
  (let* ((dir (expand-file-name directory))
         (default-directory
          (file-name-as-directory
           (if (file-directory-p dir)
               dir
             (prog1 (expand-file-name "~")
               (message "roster: directory %s not found, falling back to ~" dir))))))
    (with-temp-buffer
      (let ((status (call-process-shell-command command nil t)))
        (unless (eq status 0)
          (user-error "Command failed in %s: %s"
                      default-directory
                      (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun roster--load-sessions ()
  "Return sessions from all enabled tools as a unified list, newest-first.
Loads from OpenCode, Claude Code, and/or Codex per `roster-enabled-tools'."
  (let ((loaders `((opencode . ,#'roster--opencode-load-sessions)
                   (claude   . ,#'roster--claude-load-sessions)
                   (codex    . ,#'roster--codex-load-sessions))))
    (roster--sort-sessions
     (seq-mapcat
      (lambda (tool)
        (condition-case err
            (funcall (alist-get tool loaders))
          (error (message "roster: %s sessions unavailable: %s"
                          tool (error-message-string err))
                 nil)))
      roster-enabled-tools))))

(defun roster--read-sidecar (path)
  "Return roster metadata alist from sidecar file at PATH, or nil."
  (when (file-readable-p path)
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-key-type 'string))
          (json-read-file path))
      (error nil))))

(defun roster--write-sidecar (path title time-archived)
  "Write roster sidecar JSON to PATH.
TITLE and TIME-ARCHIVED may be nil; nil fields are omitted."
  (let ((data (append (when title `(("title" . ,title)))
                      (when time-archived `(("time_archived" . ,time-archived))))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (json-encode data)))))

;;; Terminal functions

(defun roster-open-in-ghostty (directory command)
  "Open Ghostty in DIRECTORY and run COMMAND in a new tab."
  (unless (eq system-type 'darwin)
    (user-error "roster-open-in-ghostty is only available on macOS"))
  (let* ((dir (expand-file-name directory))
         (osa (or (executable-find "osascript")
                  (user-error "osascript not found in PATH")))
         (script
          (mapconcat
           #'identity
           (list
            "tell application \"Ghostty\""
            "set cfg to new surface configuration"
            (format "set initial working directory of cfg to %s" (prin1-to-string dir))
            (format "set initial input of cfg to (character id 10) & %s & (character id 10)"
                    (prin1-to-string command))
            "if (count of windows) = 0 then"
            "new window with configuration cfg"
            "else"
            "new tab in front window with configuration cfg"
            "end if"
            "activate"
            "end tell")
           "\n")))
    (shell-command
     (format "%s -e %s"
             (shell-quote-argument osa)
             (shell-quote-argument script)))))

(defun roster-open-in-iterm (directory command)
  "Open iTerm in DIRECTORY and run COMMAND in a new tab."
  (unless (eq system-type 'darwin)
    (user-error "roster-open-in-iterm is only available on macOS"))
  (let* ((dir (expand-file-name directory))
         (shell (or (getenv "SHELL") "/bin/zsh"))
         (full-command (format "%s -lc %s"
                               (shell-quote-argument shell)
                               (shell-quote-argument
                                (format "cd %s && %s; exec %s -l"
                                        (shell-quote-argument dir)
                                        command
                                        (shell-quote-argument shell)))))
         (osa (or (executable-find "osascript")
                  (user-error "osascript not found in PATH")))
         (script
          (mapconcat
           #'identity
           (list
            "tell application \"iTerm\""
            (format "set cmd to %s" (prin1-to-string full-command))
            "activate"
            "if (count of windows) = 0 then"
            "set newWindow to (create window with default profile)"
            "tell current session of newWindow"
            "write text cmd"
            "end tell"
            "else"
            "tell current window"
            "create tab with default profile"
            "tell current session"
            "write text cmd"
            "end tell"
            "end tell"
            "end if"
            "end tell")
           "\n")))
    (shell-command
     (format "%s -e %s"
             (shell-quote-argument osa)
             (shell-quote-argument script)))))

;;; OpenCode backend

(defun roster--opencode-sql-quote (value)
  "Return SQL single-quoted VALUE with escaped apostrophes."
  (concat "'" (replace-regexp-in-string "'" "''" value t t) "'"))

(defun roster--opencode-sqlite-open ()
  "Open the OpenCode database and return a connection.
Signal `user-error' if the file is missing or the database cannot be opened."
  (unless (file-readable-p roster-opencode-db-path)
    (user-error "OpenCode database not found: %s" roster-opencode-db-path))
  (condition-case err
      (sqlite-open roster-opencode-db-path)
    (error (user-error "Cannot open OpenCode database: %s"
                       (error-message-string err)))))

(defun roster--opencode-sqlite-rows (sql)
  "Run SELECT SQL against the OpenCode database; return a list of rows.
Each row is a list of strings (NULL values become empty strings).
Returns nil when the result set is empty."
  (let ((db (roster--opencode-sqlite-open)))
    ;; `unwind-protect' ensures the connection is closed even when an error
    ;; is signalled inside the body.  `format "%s"' coerces integers and
    ;; other non-string SQL values to strings so callers get a uniform type.
    (unwind-protect
        (mapcar (lambda (row)
                  (mapcar (lambda (v) (if v (format "%s" v) "")) row))
                (sqlite-select db sql))
      (sqlite-close db))))

(defun roster--opencode-sqlite-exec-change-p (sql)
  "Run a single DML SQL statement against the OpenCode database.
Return non-nil when exactly one row was affected."
  (let ((db (roster--opencode-sqlite-open)))
    (unwind-protect
        (= 1 (sqlite-execute db sql))
      (sqlite-close db))))

(defun roster--opencode-parse-project-row (row)
  "Return project plist parsed from SQLite ROW (a list of strings)."
  (pcase-let ((`(,id ,worktree ,name) row))
    (list :id id
          :worktree (expand-file-name worktree)
          :name (unless (string-empty-p name) name))))

(defun roster--opencode-parse-session-row (row)
  "Return OpenCode session plist parsed from SQLite ROW (a list of strings)."
  (pcase-let ((`(,id ,title ,directory ,project-id ,time-updated ,archived-raw) row))
    (list :id id
          :title (if (string-empty-p title) roster--untitled title)
          :directory (expand-file-name directory)
          :project-id project-id
          :time-updated (string-to-number (or time-updated "0"))
          :time-archived (unless (string-empty-p (or archived-raw ""))
                           (string-to-number archived-raw))
          :tool 'opencode)))

(defun roster--opencode-query-projects (sql)
  "Return project plists for SQL query SQL."
  (mapcar #'roster--opencode-parse-project-row (roster--opencode-sqlite-rows sql)))

(defun roster--opencode-load-sessions ()
  "Return root OpenCode sessions as a list of plists.
Each plist has keys :id, :title, :directory, :project-id,
:time-updated, :time-archived, and :tool (always `opencode')."
  (mapcar #'roster--opencode-parse-session-row
          (roster--opencode-sqlite-rows
           (concat "SELECT id, title, directory, project_id, time_updated, "
                   "COALESCE(time_archived, '') "
                   "FROM session WHERE parent_id IS NULL ORDER BY time_updated DESC;"))))

(defun roster--opencode-project-for-directory (directory)
  "Return project plist for DIRECTORY when it matches a project worktree exactly."
  (let* ((dir (directory-file-name (expand-file-name directory)))
         (sql (concat
               "SELECT id, worktree, COALESCE(name, '') FROM project "
               "WHERE worktree = " (roster--opencode-sql-quote dir) " LIMIT 1;"))
         (projects (roster--opencode-query-projects sql)))
    (car projects)))

(defun roster--opencode-projects-containing-directory (directory)
  "Return OpenCode projects whose worktrees contain DIRECTORY."
  (let* ((dir (directory-file-name (expand-file-name directory)))
         (sql (concat
               "SELECT id, worktree, COALESCE(name, '') FROM project "
               ;; Exact match OR strict prefix: dir is longer than worktree
               ;; AND dir starts with worktree followed by '/'.  Ordered
               ;; longest-worktree-first so the most specific match is first.
               "WHERE worktree = " (roster--opencode-sql-quote dir) " "
               "OR (LENGTH(" (roster--opencode-sql-quote dir) ") > LENGTH(worktree) "
               "AND SUBSTR(" (roster--opencode-sql-quote dir) ", 1, LENGTH(worktree) + 1) = worktree || '/') "
               "ORDER BY LENGTH(worktree) DESC;")))
    (roster--opencode-query-projects sql)))

(defun roster--opencode-global-project ()
  "Return the OpenCode global project plist."
  (let* ((sql (concat
               "SELECT id, worktree, COALESCE(name, '') FROM project "
               "WHERE id = 'global' LIMIT 1;")))
    (car (roster--opencode-query-projects sql))))

(defun roster--opencode-project-label (project)
  "Return a completion label for PROJECT."
  (let ((worktree (plist-get project :worktree))
        (name (plist-get project :name)))
    (if name
        (format "%s (%s)" worktree name)
      worktree)))

(defun roster--opencode-resolve-target-project (directory)
  "Return the best OpenCode project for DIRECTORY.
Prefer an exact worktree match, otherwise fall back to a parent project whose
worktree contains DIRECTORY, and finally the global project."
  (or (roster--opencode-project-for-directory directory)
      (car (roster--opencode-projects-containing-directory directory))
      (roster--opencode-global-project)))

(defun roster--opencode-session-with-project-worktree (session-id)
  "Return session plist for SESSION-ID including its project worktree."
  (when-let ((row (car (roster--opencode-sqlite-rows
                        (concat
                         "SELECT s.id, s.title, s.directory, s.project_id, "
                         "COALESCE(p.worktree, ''), COALESCE(s.time_archived, '') "
                         "FROM session s LEFT JOIN project p ON p.id = s.project_id "
                         "WHERE s.id = " (roster--opencode-sql-quote session-id) " LIMIT 1;")))))
    (pcase-let ((`(,id ,title ,directory ,project-id ,project-worktree ,archived-raw) row))
      (list :id id
            :title (if (string-empty-p title) roster--untitled title)
            :directory (expand-file-name directory)
            :project-id project-id
            :time-archived (unless (string-empty-p archived-raw)
                             (string-to-number archived-raw))
            :project-worktree (unless (string-empty-p project-worktree)
                                (expand-file-name project-worktree))))))

(defun roster--opencode-move-session-directory (session-id directory project-id)
  "Move SESSION-ID to DIRECTORY under PROJECT-ID."
  (let ((dir (directory-file-name (expand-file-name directory))))
    (roster--opencode-sqlite-exec-change-p
     (concat "UPDATE session SET "
             "directory = " (roster--opencode-sql-quote dir) ", "
             "project_id = " (roster--opencode-sql-quote project-id) ", "
             "time_updated = CAST(unixepoch('subsec') * 1000 AS INTEGER) "
             "WHERE id = " (roster--opencode-sql-quote session-id) ";"))))

(defun roster--opencode-set-session-title (session-id title)
  "Set SESSION-ID title to TITLE.
Return non-nil when one row was updated."
  (roster--opencode-sqlite-exec-change-p
   (concat "UPDATE session SET title = " (roster--opencode-sql-quote title)
           " WHERE id = " (roster--opencode-sql-quote session-id) ";")))

(defun roster--opencode-set-session-archived (session-id archived)
  "Set SESSION-ID archived state to ARCHIVED.
Return non-nil when one row was updated."
  (let ((value (if archived
                   (number-to-string (floor (* roster--ms-per-second (float-time (current-time)))))
                 "NULL")))
    (roster--opencode-sqlite-exec-change-p
     (concat "UPDATE session SET time_archived = " value
             " WHERE id = " (roster--opencode-sql-quote session-id) ";"))))

(defun roster--opencode-delete-session (session)
  "Delete OpenCode SESSION via the official CLI workflow."
  (let* ((session-id (plist-get session :id))
         (directory (plist-get session :directory))
         (command (format "%s session delete %s"
                          roster-opencode-command
                          (shell-quote-argument session-id))))
    (roster--run-command directory command)))

;;; Claude Code backend

(defun roster--claude-jsonl-files (projects-dir)
  "Return `(ENCODED-DIR . PATH)' pairs for Claude JSONL files in PROJECTS-DIR.
Claude Code stores each project under a URL-encoded form of its absolute path
\(e.g. \"-Users-alice-myproject\"); ENCODED-DIR is that raw directory name,
used later as the key for sidecar files."
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
This is equivalent to what Claude Code's /rename command does internally."
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
  "Write roster sidecar JSON for a Claude Code session."
  (roster--write-sidecar (roster--claude-sidecar-path session-id) title time-archived))

(defun roster--claude-read-json (string)
  "Parse JSON STRING as a plist, or return nil on failure."
  (condition-case nil
      (let ((json-object-type 'plist)
            (json-key-type 'keyword)
            (json-array-type 'list)
            (json-null nil)
            (json-false nil))
        (json-read-from-string string))
    (error nil)))

(defun roster--claude-content-text (content)
  "Return the first useful text string from Claude CONTENT.
CONTENT is either a plain string (older format) or a list of part plists
\(newer format) where each part has a :type field; we look for the first
part whose :type is \"text\" and whose text is non-empty."
  (cond
   ((and (stringp content)
         (not (string-empty-p (string-trim content))))
    (string-trim content))
   ((listp content)
    ;; Use `catch'/`throw' for early exit — `seq-find' would require an
    ;; extra pass to extract the text value after finding the matching part.
    (catch 'found
      (dolist (part content)
        (when (and (listp part)
                   (equal (plist-get part :type) "text"))
          (let ((text (string-trim (or (plist-get part :text) ""))))
            (unless (string-empty-p text)
              (throw 'found text)))))
      nil))))

(defun roster--claude-update-meta-from-object (obj slug cwd title-candidate)
  "Update Claude metadata from OBJ.
Return a plist with keys :slug, :cwd, and :title-candidate.
Called once per JSONL line; treats the accumulator values (SLUG, CWD,
TITLE-CANDIDATE) as immutable — returns updated copies without mutation,
so the caller can pattern-match the result and rebind all three at once."
  (let ((new-slug (or slug (plist-get obj :slug)))
        (new-cwd cwd)
        (new-title title-candidate))
    (when (equal (plist-get obj :type) "user")
      (unless new-cwd
        (let ((value (plist-get obj :cwd)))
          (when (and (stringp value) (not (string-empty-p value)))
            (setq new-cwd value))))
      (unless new-title
        (let ((text (roster--claude-content-text
                     (plist-get (plist-get obj :message) :content))))
          ;; Skip system-injected turns such as /compact output, which Claude
          ;; Code stores as user messages beginning with an XML-like tag (e.g.
          ;; "<local-command-caveat>...").  Those are not real session starters.
          (unless (and text (string-prefix-p "<" text))
            (setq new-title text)))))
    (list :slug new-slug
          :cwd new-cwd
          :title-candidate new-title)))

(defun roster--claude-custom-title (path)
  "Return the most recent /rename title from the Claude JSONL at PATH, or nil."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-max))
        (when (re-search-backward "\"custom-title\"" nil t)
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when-let* ((obj (roster--claude-read-json line))
                        ((equal (plist-get obj :type) "custom-title"))
                        (title (plist-get obj :customTitle))
                        ((stringp title))
                        ((not (string-empty-p title))))
              title))))
    (error nil)))

(defun roster--claude-title (meta sidecar)
  "Return display title derived from META and SIDECAR."
  (let ((slug (plist-get meta :slug))
        (candidate (plist-get meta :title-candidate))
        (custom-title (plist-get meta :custom-title))
        (sidecar-title (cdr (assoc "title" sidecar))))
    (or sidecar-title
        custom-title
        slug
        (when candidate
          (if (> (length candidate) roster--title-max-length)
              (concat (substring candidate 0 (- roster--title-max-length 3)) "...")
            candidate))
        roster--untitled)))

(defun roster--claude-session-from-file (encoded-dir path)
  "Return unified Claude session plist for ENCODED-DIR and JSONL file at PATH."
  (let* ((session-id (file-name-sans-extension (file-name-nondirectory path)))
         (meta (roster--claude-parse-jsonl path)))
    (when meta
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
              :encoded-dir encoded-dir)))))

(defun roster--claude-parse-jsonl (path)
  "Return metadata plist from a Claude Code JSONL file at PATH.
Returns plist with keys :slug, :cwd, :title-candidate, :custom-title,
and :time-updated (file mtime in milliseconds)."
  (condition-case nil
      (let (slug cwd title-candidate)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (and (not (eobp))
                      (not (and slug cwd title-candidate)))
            (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                   (obj (roster--claude-read-json line)))
              (when obj
                (pcase-let ((`(:slug ,new-slug :cwd ,new-cwd :title-candidate ,new-title)
                             (roster--claude-update-meta-from-object obj slug cwd title-candidate)))
                  (setq slug new-slug
                        cwd new-cwd
                        title-candidate new-title))))
            (forward-line 1)))
        (let* ((attrs (file-attributes path))
               (mtime (when attrs (file-attribute-modification-time attrs)))
               (time-updated (if mtime
                                 (floor (* roster--ms-per-second (float-time mtime)))
                               0)))
          (list :slug slug
                :cwd (or cwd "")
                :title-candidate title-candidate
                :custom-title (roster--claude-custom-title path)
                :time-updated time-updated)))
    (error nil)))

(defun roster--claude-load-sessions ()
  "Return Claude Code sessions as a list of unified session plists."
  (let ((projects-dir (roster--claude-projects-dir)))
    (when (file-directory-p projects-dir)
      (delq nil
            (mapcar (lambda (entry)
                      (roster--claude-session-from-file (car entry) (cdr entry)))
                    (roster--claude-jsonl-files projects-dir))))))

(defun roster--claude-delete-session (session)
  "Delete a Claude Code SESSION's JSONL file and roster sidecar."
  (let* ((session-id (plist-get session :id))
         (encoded-dir (plist-get session :encoded-dir))
         (dir (expand-file-name encoded-dir (roster--claude-projects-dir)))
         (jsonl (expand-file-name (concat session-id ".jsonl") dir))
         (sidecar (roster--claude-sidecar-path session-id)))
    (when (file-exists-p jsonl)
      (move-file-to-trash jsonl))
    (when (file-exists-p sidecar)
      (delete-file sidecar))))

(defun roster--claude-rename-session-command (session)
  "Rename a Claude Code SESSION by appending a custom-title record to its JSONL.
This is equivalent to /rename inside Claude Code; return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (encoded-dir (plist-get session :encoded-dir))
         (old-title (roster--session-title session))
         (new-title (roster--read-session-title session)))
    (if (string= old-title new-title)
        (progn (message "Session %s already uses that title" session-id) nil)
      (roster--claude-append-custom-title encoded-dir session-id new-title)
      (message "Renamed session %s to %s" session-id new-title)
      t)))

(defun roster--claude-do-archive (session archived)
  "Set a Claude Code SESSION archived state to ARCHIVED without prompting."
  (let* ((session-id (plist-get session :id))
         (encoded-dir (plist-get session :encoded-dir))
         (new-archived (when archived
                         (floor (* roster--ms-per-second (float-time (current-time)))))))
    (roster--claude-write-sidecar session-id nil new-archived)))

(defun roster--claude-set-archived-command (session archived)
  "Set a Claude Code SESSION archived state to ARCHIVED; return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (title (plist-get session :title))
         (verb (if archived "Archive" "Unarchive"))
         (past (if archived "Archived" "Unarchived")))
    (when (yes-or-no-p (format "%s Claude session '%s' (%s)? " verb title session-id))
      (roster--claude-do-archive session archived)
      (message "%s session %s" past session-id)
      t)))

;;; Codex backend

(defun roster--codex-sessions-dir ()
  "Return the Codex active sessions directory."
  (expand-file-name "sessions" roster-codex-dir))

(defun roster--codex-archived-dir ()
  "Return the Codex archived sessions directory."
  (expand-file-name "archived_sessions" roster-codex-dir))

(defun roster--codex-roster-dir ()
  "Return the directory used to store Codex roster sidecar files."
  (expand-file-name "roster" roster-codex-dir))

(defun roster--codex-sidecar-path (session-id)
  "Return the path to the roster sidecar JSON file for a Codex session."
  (expand-file-name (concat session-id ".roster.json")
                    (roster--codex-roster-dir)))

(defun roster--codex-read-sidecar (session-id)
  "Return roster metadata alist for a Codex session, or nil if no sidecar."
  (roster--read-sidecar (roster--codex-sidecar-path session-id)))

(defun roster--codex-write-sidecar (session-id title time-archived)
  "Write roster sidecar JSON for a Codex session."
  (roster--write-sidecar (roster--codex-sidecar-path session-id) title time-archived))

(defun roster--codex-date-path-from-filename (filename)
  "Return YYYY/MM/DD path extracted from a Codex JSONL filename.
Filename format: rollout-YYYY-MM-DDTHH-MM-SS-UUID.jsonl.
Returns nil when the format is not recognized."
  (when (string-match
         "rollout-\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T"
         filename)
    (format "%s/%s/%s"
            (match-string 1 filename)
            (match-string 2 filename)
            (match-string 3 filename))))

(defun roster--codex-jsonl-files ()
  "Return a list of (PATH . ARCHIVED) for all Codex JSONL files."
  (let ((sessions-dir (roster--codex-sessions-dir))
        (archived-dir (roster--codex-archived-dir))
        result)
    (when (file-directory-p sessions-dir)
      (dolist (path (directory-files-recursively sessions-dir "\\.jsonl\\'"))
        (push (cons path nil) result)))
    (when (file-directory-p archived-dir)
      (dolist (path (directory-files-recursively archived-dir "\\.jsonl\\'"))
        (push (cons path t) result)))
    (nreverse result)))

(defun roster--codex-parse-jsonl (path)
  "Return metadata plist from a Codex JSONL file at PATH.
Returns plist with keys :id, :cwd, :title-candidate, and :time-updated.
Reads the file line by line and stops as soon as all three metadata fields
are found, so arbitrarily large preambles (e.g. long system prompts) are
handled without any fixed byte limit."
  (condition-case nil
      (let (session-id cwd title-candidate)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (and (not (eobp))
                      (not (and session-id cwd title-candidate)))
            (when-let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                        (obj (roster--claude-read-json line))
                        (type (plist-get obj :type))
                        (payload (plist-get obj :payload)))
              (pcase type
                ("session_meta"
                 (unless session-id
                   (when-let* ((v (plist-get payload :id)) ((stringp v)))
                     (setq session-id v)))
                 (unless cwd
                   (when-let* ((v (plist-get payload :cwd))
                               ((stringp v)) ((not (string-empty-p v))))
                     (setq cwd v))))
                ("event_msg"
                 (when (and (not title-candidate)
                            (equal (plist-get payload :type) "user_message"))
                   (let ((msg (string-trim (or (plist-get payload :message) ""))))
                     (unless (string-empty-p msg)
                       (setq title-candidate msg)))))))
            (forward-line 1)))
        (let* ((attrs (file-attributes path))
               (mtime (when attrs (file-attribute-modification-time attrs)))
               (time-updated (if mtime (floor (* roster--ms-per-second (float-time mtime))) 0)))
          (list :id session-id
                :cwd (or cwd "")
                :title-candidate title-candidate
                :time-updated time-updated)))
    (error nil)))

(defun roster--codex-session-from-file (path archived)
  "Return a unified Codex session plist for JSONL file at PATH.
ARCHIVED is non-nil when PATH is from the archived_sessions directory."
  (when-let* ((meta (roster--codex-parse-jsonl path))
              (session-id (plist-get meta :id)))
    (let* ((sidecar (roster--codex-read-sidecar session-id))
           (cwd (plist-get meta :cwd))
           (dir (expand-file-name (if (string-empty-p cwd) "~" cwd)))
           (sidecar-title (when sidecar (cdr (assoc "title" sidecar))))
           (title
            (or sidecar-title
                (when-let ((candidate (plist-get meta :title-candidate)))
                  (if (> (length candidate) roster--title-max-length)
                      (concat (substring candidate 0 (- roster--title-max-length 3)) "...")
                    candidate))
                (let ((base (file-name-nondirectory (directory-file-name dir))))
                  (unless (string-empty-p base) base))
                roster--untitled))
           (time-archived
            (when archived
              (let ((v (when sidecar (cdr (assoc "time_archived" sidecar)))))
                (if (numberp v) v 1)))))
      (list :id session-id
            :title title
            :directory dir
            :project-id nil
            :time-updated (plist-get meta :time-updated)
            :time-archived time-archived
            :tool 'codex
            :file-path path))))

(defun roster--codex-load-sessions ()
  "Return Codex sessions as a list of unified session plists."
  (when (file-directory-p roster-codex-dir)
    (seq-keep (lambda (entry)
                (roster--codex-session-from-file (car entry) (cdr entry)))
              (roster--codex-jsonl-files))))

(defun roster--codex-shell-snapshot-path (session-id)
  "Return the path to the Codex shell snapshot file for SESSION-ID."
  (expand-file-name (concat session-id ".sh")
                    (expand-file-name "shell_snapshots" roster-codex-dir)))

(defun roster--codex-delete-session (session)
  "Delete a Codex SESSION's JSONL file, shell snapshot, and roster sidecar."
  (let* ((session-id (plist-get session :id))
         (file-path (plist-get session :file-path))
         (snapshot (roster--codex-shell-snapshot-path session-id))
         (sidecar (roster--codex-sidecar-path session-id)))
    (when (file-exists-p file-path)
      (move-file-to-trash file-path))
    (when (file-exists-p snapshot)
      (move-file-to-trash snapshot))
    (when (file-exists-p sidecar)
      (delete-file sidecar))))

(defun roster--codex-rename-session-command (session)
  "Rename a Codex SESSION via its roster sidecar; return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (old-title (roster--session-title session))
         (new-title (roster--read-session-title session)))
    (if (string= old-title new-title)
        (progn (message "Session %s already uses that title" session-id) nil)
      (let ((sidecar (roster--codex-read-sidecar session-id)))
        (roster--codex-write-sidecar
         session-id
         new-title
         (when sidecar (cdr (assoc "time_archived" sidecar))))
        (message "Renamed session %s to %s" session-id new-title)
        t))))

(defun roster--codex-do-archive (session archived)
  "Set a Codex SESSION archived state to ARCHIVED without prompting."
  (let* ((session-id (plist-get session :id))
         (file-path (plist-get session :file-path))
         (fname (file-name-nondirectory file-path))
         (dest-dir (if archived
                       (roster--codex-archived-dir)
                     (let ((date-path (roster--codex-date-path-from-filename fname)))
                       (if date-path
                           (expand-file-name date-path (roster--codex-sessions-dir))
                         (roster--codex-sessions-dir)))))
         (sidecar (roster--codex-read-sidecar session-id))
         (sidecar-title (when sidecar (cdr (assoc "title" sidecar)))))
    (make-directory dest-dir t)
    (rename-file file-path (expand-file-name fname dest-dir))
    (roster--codex-write-sidecar
     session-id sidecar-title
     (when archived (floor (* roster--ms-per-second (float-time (current-time))))))))

(defun roster--codex-set-archived-command (session archived)
  "Set a Codex SESSION archived state to ARCHIVED; return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (title (plist-get session :title))
         (verb (if archived "Archive" "Unarchive"))
         (past (if archived "Archived" "Unarchived")))
    (when (yes-or-no-p (format "%s Codex session '%s' (%s)? " verb title session-id))
      (roster--codex-do-archive session archived)
      (message "%s Codex session %s" past session-id)
      t)))

;;; Tool helpers

(defun roster--tool-label (session)
  "Return the short tool tag string for SESSION."
  (pcase (roster--session-tool session)
    ('claude "CC")
    ('codex  "CX")
    (_       "OC")))

(defun roster--tool-face (session)
  "Return the face for SESSION's tool tag."
  (pcase (roster--session-tool session)
    ('claude 'roster-list-tool-claude-face)
    ('codex  'roster-list-tool-codex-face)
    (_       'roster-list-tool-opencode-face)))

(defun roster--session-command (session)
  "Return the shell command used to resume SESSION."
  (pcase (roster--session-tool session)
    ('claude
     (format "%s -r %s"
             roster-claude-command
             (shell-quote-argument (roster--session-id session))))
    ('codex
     (format "%s resume %s"
             roster-codex-command
             (shell-quote-argument (roster--session-id session))))
    (_
     (format "%s -s %s"
             roster-opencode-command
             (shell-quote-argument (roster--session-id session))))))

(defun roster--new-session-command (tool)
  "Return the command used to create a new TOOL session."
  (pcase tool
    ('claude roster-claude-command)
    ('codex  roster-codex-command)
    (_ roster-opencode-command)))

(defun roster--select-tool-for-new-session ()
  "Return the tool symbol to use for a new session."
  (if (cdr roster-enabled-tools)
      (intern (completing-read
               "Tool: "
               (mapcar #'symbol-name roster-enabled-tools)
               nil t nil nil
               (symbol-name roster-default-new-session-tool)))
    (or (car roster-enabled-tools) 'opencode)))

(defun roster--session-labels (sessions)
  "Build completion labels for SESSIONS.
Labels use title only. Duplicate titles are numbered as (1), (2), (3)."
  (let ((totals (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        labels)
    (dolist (s sessions)
      (let ((title (roster--session-display-title s)))
        (puthash title (1+ (gethash title totals 0)) totals)))
    (dolist (s sessions)
      (let* ((title (roster--session-display-title s))
             (total (gethash title totals 0))
             (idx (1+ (gethash title seen 0)))
             (label (if (> total 1)
                        (format "%s (%d)" title idx)
                      title)))
        (puthash title idx seen)
        (push (cons label s) labels)))
    (nreverse labels)))

(defun roster--ensure-session-title (title)
  "Return trimmed TITLE or signal a `user-error'."
  (let ((value (string-trim title)))
    (when (string-empty-p value)
      (user-error "Session title cannot be empty"))
    value))

(defun roster--read-session-title (session)
  "Prompt for a new title for SESSION and return it.
The return value is trimmed and guaranteed non-empty."
  (roster--ensure-session-title
   (read-string (format "Rename session (%s): " (roster--session-title session))
                (roster--session-title session))))

(defun roster--session-annotation (candidate)
  "Return shadow annotation for completion CANDIDATE."
  (let ((dir (alist-get candidate roster--completion-directory-map nil nil #'string=)))
    (when dir
      (concat " " (propertize dir 'face 'shadow)))))

(defun roster--select-session (sessions prompt)
  "Ask user to select from SESSIONS with PROMPT.
Return the selected session plist."
  (let* ((table (roster--session-labels sessions))
         (roster--completion-directory-map
          (mapcar (lambda (pair)
                    (cons (car pair) (plist-get (cdr pair) :directory)))
                  table))
         (completion-extra-properties
          '(:annotation-function roster--session-annotation))
         (choice (completing-read prompt table nil t)))
    (cdr (assoc choice table))))

;;; Project scoping

(defun roster--directory-prefix-p (dir parent)
  "Return non-nil when DIR is within PARENT."
  (string-prefix-p (file-name-as-directory (expand-file-name parent))
                   (file-name-as-directory (expand-file-name dir))))

(defun roster--project-scope-directory ()
  "Return project root for current directory, or current directory itself.
If `default-directory' belongs to a project, return that project root;
otherwise return `default-directory'."
  (let* ((proj (project-current nil default-directory))
         (root (when proj (project-root proj))))
    (expand-file-name (or root default-directory))))

(defun roster--project-scoped-sessions (sessions)
  "Return SESSIONS within the current project scope."
  (let ((scope (roster--project-scope-directory)))
    (seq-filter
     (lambda (session)
       (roster--directory-prefix-p (plist-get session :directory) scope))
     sessions)))

(defun roster--start-new-session-with-directory-prompt ()
  "Prompt for a directory and optional tool, then start a new session."
  (let* ((dir (read-directory-name "Directory for new session: "
                                   default-directory nil t))
         (tool (roster--select-tool-for-new-session)))
    (funcall roster-terminal-function dir (roster--new-session-command tool))))

(defun roster--resume-session (session &optional jump)
  "Resume SESSION in a terminal window.
When JUMP is non-nil, open the session directory in Dired first."
  (let ((directory (roster--session-directory session)))
    (when jump
      (dired directory))
    (funcall roster-terminal-function directory (roster--session-command session))))

;;; List mode internals

(defun roster--list-sessions ()
  "Return sessions for the current `roster' list buffer."
  (unless roster-list-source-function
    (user-error "No session source configured for this roster buffer"))
  (let ((sessions (funcall roster-list-source-function)))
    (if roster-list-show-archived
        sessions
      (roster--active-sessions sessions))))

(defun roster--list-entry (session)
  "Build one tabulated list entry for SESSION."
  (let ((directory (plist-get session :directory)))
    (list (plist-get session :id)
          (vector
           (propertize (replace-regexp-in-string "[[:cntrl:]]" " "
                                               (roster--session-title session))
                       'face 'roster-list-title-face)
           (propertize (roster--tool-label session) 'face (roster--tool-face session))
           (propertize (upcase (roster--session-state session)) 'face (roster--state-face session))
           (propertize (file-name-nondirectory (directory-file-name directory))
                       'face 'roster-list-project-face)
           (propertize directory 'face 'roster-list-directory-face)
           (propertize (roster--format-time-millis (plist-get session :time-updated))
                       'face 'roster-list-time-face)))))

(defun roster--list-refresh ()
  "Refresh `tabulated-list-entries' for the current `roster' buffer."
  (setq tabulated-list-entries
        (mapcar #'roster--list-entry (roster--list-sessions))))

(defun roster--session-at-point ()
  "Return the session at point in a `roster-list-mode' buffer."
  (let ((session-id (tabulated-list-get-id)))
    (unless session-id
      (user-error "No session on this line"))
    (or (roster--session-by-id session-id)
        (user-error "Session %s no longer exists" session-id))))

(defun roster-list-refresh ()
  "Refresh the current `roster' list buffer."
  (interactive)
  (revert-buffer)
  (roster-list--apply-marks))

(defun roster-list-toggle-archived ()
  "Toggle whether archived sessions are shown in the current list."
  (interactive)
  (setq roster-list-show-archived (not roster-list-show-archived))
  (tabulated-list-revert)
  (message "%s archived sessions"
           (if roster-list-show-archived "Showing" "Hiding")))

(defun roster-list-resume (&optional arg)
  "Resume the session on the current line.
With a prefix argument, open the session directory in Dired first."
  (interactive "P")
  (roster--resume-session (roster--session-at-point) arg))

(defun roster-list-open-directory ()
  "Open the current session's directory in Dired."
  (interactive)
  (dired (plist-get (roster--session-at-point) :directory)))

(defun roster-list-rename ()
  "Rename the session on the current line."
  (interactive)
  (when (roster--rename-session-command (roster--session-at-point))
    (tabulated-list-revert)))

(defun roster-list-toggle-archive ()
  "Toggle archived state for the session on the current line."
  (interactive)
  (let* ((session (roster--session-at-point))
         (archived (not (roster--session-archived-p session))))
    (when (roster--opencode-set-session-archived-command session archived)
      (tabulated-list-revert))))

(defun roster-list-move-directory ()
  "Move the session on the current line to another project directory."
  (interactive)
  (when (roster--opencode-update-session-directory-command (roster--session-at-point))
    (tabulated-list-revert)))

(defun roster-list-delete ()
  "Delete the session on the current line."
  (interactive)
  (let ((line (line-number-at-pos)))
    (when (roster--delete-session-command (roster--session-at-point))
      (tabulated-list-revert)
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (when (eobp)
        (forward-line -1)))))

(defun roster-list-new-session ()
  "Start a new session from the list buffer."
  (interactive)
  (roster--start-new-session-with-directory-prompt))

;;; Mark and bulk operations

(defun roster-list--marked-ids ()
  "Return the list of marked session IDs in the current buffer."
  (let (ids)
    (maphash (lambda (id _) (push id ids)) roster-list--marked)
    (nreverse ids)))

(defun roster-list--delete-mark-overlays (ovs)
  "Delete overlay pair OVS (a cons of two overlays)."
  (when (overlayp (car ovs)) (delete-overlay (car ovs)))
  (when (overlayp (cdr ovs)) (delete-overlay (cdr ovs))))

(defun roster-list--clear-marks ()
  "Remove all marks and their overlays in the current buffer."
  (maphash (lambda (_id ovs) (roster-list--delete-mark-overlays ovs))
           roster-list--mark-overlays)
  (clrhash roster-list--marked)
  (clrhash roster-list--mark-overlays))

(defun roster-list--add-mark-overlay (session-id)
  "Highlight the current line as marked for SESSION-ID."
  (when-let ((existing (gethash session-id roster-list--mark-overlays)))
    (roster-list--delete-mark-overlays existing))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position)))
        (mark-ov (make-overlay (line-beginning-position)
                               (1+ (line-beginning-position)))))
    (overlay-put ov 'face 'roster-list-mark-face)
    (overlay-put mark-ov 'display (propertize "*" 'face 'roster-list-mark-indicator-face))
    (puthash session-id (cons ov mark-ov) roster-list--mark-overlays)))

(defun roster-list--apply-marks ()
  "Reapply mark overlays after a buffer refresh.
After `revert-buffer' the buffer is repopulated and all overlay positions
are stale.  We drop every existing overlay, then walk the new buffer to
rebuild them at their current line positions."
  (maphash (lambda (_id ovs) (roster-list--delete-mark-overlays ovs))
           roster-list--mark-overlays)
  (clrhash roster-list--mark-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((id (tabulated-list-get-id)))
        (when (and id (gethash id roster-list--marked))
          (roster-list--add-mark-overlay id)))
      (forward-line 1))))

(defun roster-list--nearest-surviving-session (deleted-ids)
  "Return the ID of the session nearest to point not in DELETED-IDS.
Prefers a following line when two candidates are equidistant.
Used to decide where to land point after a bulk delete so the viewport
jumps as little as possible."
  (let* ((origin (line-number-at-pos))
         (best-id nil)
         (best-dist nil)
         (best-fwd nil))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((id (tabulated-list-get-id)))
          (when (and id (not (member id deleted-ids)))
            (let* ((ln (line-number-at-pos))
                   (d (abs (- ln origin)))
                   (fwd (>= ln origin)))
              ;; Prefer closer; break ties by choosing the forward direction
              ;; (i.e. the next row below point rather than the one above).
              (when (or (null best-dist)
                        (< d best-dist)
                        (and (= d best-dist) fwd (not best-fwd)))
                (setq best-id id best-dist d best-fwd fwd)))))
        (forward-line 1)))
    best-id))

(defun roster-list--line-of-session (session-id)
  "Return the line number of SESSION-ID in the current buffer, or nil."
  (when session-id
    (save-excursion
      (goto-char (point-min))
      (let (line)
        (while (and (not line) (< (point) (point-max)))
          (when (equal (tabulated-list-get-id) session-id)
            (setq line (line-number-at-pos)))
          (forward-line 1))
        line))))

(defun roster-list-mark ()
  "Toggle mark on the session at point and advance to the next line.
With an active region, mark all sessions in the region (no toggle)."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             ;; When the region ends exactly at the beginning of a line the
             ;; user visually selected up to but not including that line.
             ;; Back up by one char so the loop does not mark that extra row.
             (finish (if (and (> end beg)
                              (save-excursion (goto-char end) (bolp)))
                         (1- end)
                       end)))
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (while (<= (line-beginning-position) finish)
            (when-let ((id (tabulated-list-get-id)))
              (puthash id t roster-list--marked)
              (roster-list--add-mark-overlay id))
            (forward-line 1)))
        (deactivate-mark)
        (goto-char (max beg end))
        (beginning-of-line)
        (forward-line 1))
    (let ((id (tabulated-list-get-id)))
      (unless id (user-error "No session on this line"))
      (if (gethash id roster-list--marked)
          (progn
            (remhash id roster-list--marked)
            (when-let ((ovs (gethash id roster-list--mark-overlays)))
              (roster-list--delete-mark-overlays ovs)
              (remhash id roster-list--mark-overlays)))
        (puthash id t roster-list--marked)
        (roster-list--add-mark-overlay id))
      (forward-line 1))))

(defun roster-list-unmark ()
  "Unmark the session at point and advance to the next line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session on this line"))
    (remhash id roster-list--marked)
    (when-let ((ovs (gethash id roster-list--mark-overlays)))
      (roster-list--delete-mark-overlays ovs)
      (remhash id roster-list--mark-overlays))
    (forward-line 1)))

(defun roster-list-unmark-all ()
  "Clear all marks in the current roster list buffer."
  (interactive)
  (roster-list--clear-marks)
  (message "Cleared all marks"))

(defun roster-list-delete-marked ()
  "Delete all marked sessions after confirmation.
If no sessions are marked, delete the session at point (same as `d')."
  (interactive)
  (let ((ids (roster-list--marked-ids)))
    (if (null ids)
        (roster-list-delete)
      (when (yes-or-no-p (format "Delete %d marked sessions? " (length ids)))
        (let* (;; Capture the visual row offset of point within the window so
               ;; we can `recenter' to the same screen position after refresh.
               (win-line (count-screen-lines (window-start) (point)))
               (target-id (roster-list--nearest-surviving-session ids))
               (all-sessions (roster--load-sessions)))
          (dolist (id ids)
            (when-let ((session (seq-find (lambda (s) (string= (plist-get s :id) id))
                                          all-sessions)))
              (roster--do-delete-session session)))
          (roster-list--clear-marks)
          (revert-buffer)
          (roster-list--apply-marks)
          (when-let ((ln (roster-list--line-of-session target-id)))
            (goto-char (point-min))
            (forward-line (1- ln))
            (recenter win-line))
          (message "Deleted %d sessions" (length ids)))))))

(defun roster-list-archive-marked ()
  "Toggle archive state of all marked sessions after confirmation.
If no sessions are marked, toggle archive state of the session at point
\(same as `a')."
  (interactive)
  (let ((ids (roster-list--marked-ids)))
    (if (null ids)
        (roster-list-toggle-archive)
      (let* ((all-sessions (roster--load-sessions))
             (sessions (seq-keep (lambda (id)
                                   (seq-find (lambda (s) (string= (plist-get s :id) id))
                                             all-sessions))
                                 ids))
             (n-archive   (seq-count (lambda (s) (not (roster--session-archived-p s))) sessions))
             (n-unarchive (seq-count #'roster--session-archived-p sessions))
             (verb (cond ((zerop n-unarchive) "Archive")
                         ((zerop n-archive)   "Unarchive")
                         (t "Archive/Unarchive")))
             (past (cond ((zerop n-unarchive) "Archived")
                         ((zerop n-archive)   "Unarchived")
                         (t "Archived/Unarchived"))))
        (when (yes-or-no-p (format "%s %d marked sessions? " verb (length sessions)))
          (let* ((win-line (count-screen-lines (window-start) (point)))
                 (target-id (roster-list--nearest-surviving-session ids)))
            (dolist (session sessions)
              (roster--do-archive-session session
                                          (not (roster--session-archived-p session))))
            (roster-list--clear-marks)
            (revert-buffer)
            (roster-list--apply-marks)
            (when-let ((ln (roster-list--line-of-session target-id)))
              (goto-char (point-min))
              (forward-line (1- ln))
              (recenter win-line))
            (message "%s %d sessions" past (length sessions))))))))

;;; List mode definition

(defvar roster-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'roster-list-resume)
    (define-key map (kbd "e") #'roster-list-resume)
    (define-key map (kbd "d") #'roster-list-delete)
    (define-key map (kbd "r") #'roster-list-rename)
    (define-key map (kbd "a") #'roster-list-toggle-archive)
    (define-key map (kbd "R") #'roster-list-move-directory)
    (define-key map (kbd "o") #'roster-list-open-directory)
    (define-key map (kbd "c") #'roster-list-new-session)
    (define-key map (kbd "g") #'roster-list-refresh)
    (define-key map (kbd "t") #'roster-list-toggle-archived)
    (define-key map (kbd "m") #'roster-list-mark)
    (define-key map (kbd "u") #'roster-list-unmark)
    (define-key map (kbd "U") #'roster-list-unmark-all)
    (define-key map (kbd "D") #'roster-list-delete-marked)
    (define-key map (kbd "A") #'roster-list-archive-marked)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `roster-list-mode'.")

(define-derived-mode roster-list-mode tabulated-list-mode "roster"
  "Major mode for managing AI coding sessions."
  (setq tabulated-list-format [("Title"     28 t)
                               ("Tool"       4 t)
                               ("State"     10 t)
                               ("Project"   18 t)
                               ("Directory" 38 t)
                               ("Updated"   16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'roster--list-refresh nil t)
  (tabulated-list-init-header))

(defun roster--open-list-buffer (buffer-name source-function &optional include-archived)
  "Open a `roster' list BUFFER-NAME using SOURCE-FUNCTION.
When INCLUDE-ARCHIVED is non-nil, archived sessions are shown initially.
When omitted or nil, the value of `roster-list-include-archived' is used."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (roster-list-mode)
      (setq-local roster-list-source-function source-function)
      (setq-local roster-list-show-archived (if (null include-archived)
                                                roster-list-include-archived
                                              include-archived))
      (tabulated-list-revert))
    (pop-to-buffer buffer)))

;;; Command dispatch

(defun roster--opencode-rename-session-command (session)
  "Rename an OpenCode SESSION via SQLite and return non-nil when changed."
  (let* ((session-id (plist-get session :id))
         (old-title (roster--session-title session))
         (new-title (roster--read-session-title session)))
    (if (string= old-title new-title)
        (progn
          (message "Session %s already uses that title" session-id)
          nil)
      (unless (roster--opencode-set-session-title session-id new-title)
        (user-error "No session updated for id %s" session-id))
      (let ((updated (roster--opencode-session-with-project-worktree session-id)))
        (unless (and updated
                     (string= (plist-get updated :title) new-title))
          (user-error "Session %s failed title verification" session-id))
        (message "Renamed session %s to %s" session-id new-title)
        t))))

(defun roster--rename-session-command (session)
  "Rename SESSION and return non-nil when the title changes."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-rename-session-command session))
    ('codex  (roster--codex-rename-session-command session))
    (_       (roster--opencode-rename-session-command session))))

(defun roster--opencode-do-archive (session archived)
  "Set an OpenCode SESSION archived state to ARCHIVED without prompting."
  (let* ((session-id (roster--session-id session))
         (verb (if archived "Archive" "Unarchive")))
    (unless (roster--opencode-set-session-archived session-id archived)
      (user-error "No session updated for id %s" session-id))
    (let ((updated (roster--opencode-session-with-project-worktree session-id)))
      (unless (and updated (eq (roster--session-archived-p updated) archived))
        (user-error "Session %s failed %s verification" session-id (downcase verb)))
      t)))

(defun roster--opencode-set-archived-command (session archived)
  "Set an OpenCode SESSION archived state to ARCHIVED; return non-nil on change."
  (let* ((session-id (roster--session-id session))
         (title (roster--session-title session))
         (verb (if archived "Archive" "Unarchive"))
         (past (if archived "Archived" "Unarchived")))
    (when (yes-or-no-p (format "%s OpenCode session '%s' (%s)? " verb title session-id))
      (roster--opencode-do-archive session archived)
      (message "%s session %s" past session-id)
      t)))

(defun roster--opencode-set-session-archived-command (session archived)
  "Set SESSION archived state to ARCHIVED and return non-nil on change."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-set-archived-command session archived))
    ('codex  (roster--codex-set-archived-command session archived))
    (_       (roster--opencode-set-archived-command session archived))))

(defun roster--do-delete-session (session)
  "Delete SESSION without prompting."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-delete-session session))
    ('codex  (roster--codex-delete-session session))
    (_       (roster--opencode-delete-session session))))

(defun roster--do-archive-session (session archived)
  "Archive or unarchive SESSION without prompting."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-do-archive session archived))
    ('codex  (roster--codex-do-archive session archived))
    (_       (roster--opencode-do-archive session archived))))

(defun roster--delete-session-command (session)
  "Delete SESSION and return non-nil on success."
  (let ((session-id (roster--session-id session))
        (title (roster--session-title session))
        (directory (roster--session-directory session)))
    (when (yes-or-no-p (format "Delete session '%s' (%s)? " title session-id))
      (roster--do-delete-session session)
      (message "Deleted session %s from %s" session-id directory)
      t)))

(defun roster--opencode-verify-moved-session (session-id directory project-id project-worktree)
  "Signal when SESSION-ID does not match DIRECTORY and PROJECT-ID.
PROJECT-WORKTREE is the expected resolved worktree for PROJECT-ID."
  (let ((updated (roster--opencode-session-with-project-worktree session-id)))
    (unless updated
      (user-error "Updated session %s could not be reloaded" session-id))
    (unless (and (string= (plist-get updated :directory) directory)
                 (string= (plist-get updated :project-id) project-id)
                 (string= (or (plist-get updated :project-worktree) "") project-worktree))
      (user-error "Session %s failed post-update consistency checks" session-id))))

(defun roster--opencode-target-project-for-directory (directory)
  "Return the resolved OpenCode project for DIRECTORY or signal a `user-error'."
  (or (roster--opencode-resolve-target-project directory)
      (user-error
       (concat
        "No OpenCode project matches %s. OpenCode only stays consistent when the target "
        "directory already exists as a project worktree.")
       directory)))

(defun roster--opencode-move-session-confirmed-p (session-id old-dir new-dir)
  "Return non-nil when the user confirms moving SESSION-ID from OLD-DIR to NEW-DIR."
  (yes-or-no-p (format "Move session %s from %s to %s? " session-id old-dir new-dir)))

(defun roster--opencode-update-session-directory-command (session)
  "Move SESSION to another known OpenCode project directory.
Signals a `user-error' for Claude Code and Codex sessions, which are not movable."
  (when (memq (plist-get session :tool) '(claude codex))
    (user-error "Directory moves are not supported for %s sessions"
                (pcase (plist-get session :tool)
                  ('claude "Claude Code")
                  ('codex  "Codex"))))
  (let* ((session-id (plist-get session :id))
         (old-dir (plist-get session :directory))
         (old-project-id (plist-get session :project-id))
         (new-dir (directory-file-name
                   (expand-file-name
                    (read-directory-name (format "New directory (current: %s): " old-dir)
                                         old-dir nil t)))))
    (unless (file-directory-p new-dir)
      (user-error "Directory does not exist: %s" new-dir))
    (let* ((target-project (roster--opencode-target-project-for-directory new-dir))
           (new-project-id (plist-get target-project :id)))
      (cond
       ((and (string= old-dir new-dir)
             (string= old-project-id new-project-id))
        (message "Session %s already points to %s" session-id new-dir)
        nil)
       ((not (roster--opencode-move-session-confirmed-p session-id old-dir new-dir))
        nil)
       (t
        (unless (roster--opencode-move-session-directory session-id new-dir new-project-id)
          (user-error "No session updated for id %s" session-id))
        (roster--opencode-verify-moved-session session-id new-dir new-project-id
                                               (plist-get target-project :worktree))
        (message
         "Moved session %s to %s. Restart active OpenCode views if they still show stale state."
         session-id new-dir)
        t)))))

;;; Public commands

;;;###autoload
(defun roster-update-session-directory ()
  "Safely move a session to another known OpenCode project directory.

This updates both `session.directory' and `session.project_id', and only
allows targets that already exist in the OpenCode `project' table."
  (interactive)
  (roster--opencode-update-session-directory-command
   (roster--select-from-sessions
    (roster--load-sessions)
    "Select OpenCode session to move: "
    "No OpenCode sessions found")))

;;;###autoload
(defun roster-rename-session ()
  "Rename an existing session."
  (interactive)
  (roster--rename-session-command
   (roster--select-from-sessions
    (roster--active-sessions (roster--load-sessions))
    "Select session to rename: "
    "No active sessions found")))

;;;###autoload
(defun roster-archive-session ()
  "Archive an active session."
  (interactive)
  (roster--opencode-set-session-archived-command
   (roster--select-from-sessions
    (roster--active-sessions (roster--load-sessions))
    "Select session to archive: "
    "No active sessions found")
   t))

;;;###autoload
(defun roster-unarchive-session ()
  "Unarchive an existing session."
  (interactive)
  (roster--opencode-set-session-archived-command
   (roster--select-from-sessions
    (roster--archived-sessions (roster--load-sessions))
    "Select session to unarchive: "
    "No archived sessions found")
   nil))

;;;###autoload
(defun roster-delete-session ()
  "Delete an existing session."
  (interactive)
  (roster--delete-session-command
   (roster--select-from-sessions
    (roster--load-sessions)
    "Select session to delete: "
    "No sessions found")))

;;;###autoload
(defun roster-list-sessions ()
  "Open a Dired-like buffer for managing sessions."
  (interactive)
  (roster--open-list-buffer roster-list-buffer-name #'roster--load-sessions))

;;;###autoload
(defun roster-list-project-sessions ()
  "Open a Dired-like buffer for sessions in the current project scope."
  (interactive)
  (let ((scope (roster--project-scope-directory)))
    (roster--open-list-buffer
     (format "%s<%s>" roster-list-buffer-name (file-name-nondirectory (directory-file-name scope)))
     (lambda ()
       (roster--project-scoped-sessions (roster--load-sessions))))))

;;;###autoload
(defun roster-open-session (&optional arg)
  "Choose a session and resume it.
With a prefix argument, open the session directory in Dired first.
If there are no sessions, prompt for a directory and start a new one."
  (interactive "P")
  (let ((sessions (roster--load-sessions)))
    (if sessions
        (roster--resume-session
         (roster--select-session sessions "Select session to resume: ")
         arg)
      (roster--start-new-session-with-directory-prompt))))

;;;###autoload
(defun roster-open-session-project (&optional arg)
  "Choose and resume a session under the current project scope.
If current directory belongs to a project, scope is project root and subdirs.
Otherwise scope is current directory and subdirs.
With a prefix argument, open the session directory in Dired first.
If no matching session exists, prompt for a directory and start a new one."
  (interactive "P")
  (let* ((sessions (roster--load-sessions))
         (scope (roster--project-scope-directory))
         (scoped-sessions (roster--project-scoped-sessions sessions)))
    (if scoped-sessions
        (roster--resume-session
         (roster--select-session scoped-sessions
                                 (format "Select session in project scope (%s): " scope))
         arg)
      (roster--start-new-session-with-directory-prompt))))

;;;###autoload
(defun roster-open-latest-session-project ()
  "Resume the latest active session in the current project scope.
If none exists, prompt for a directory and start a new one."
  (interactive)
  (let* ((sessions (roster--load-sessions))
         (scoped-sessions (roster--active-sessions
                           (roster--project-scoped-sessions sessions))))
    (if scoped-sessions
        (roster--resume-session (car scoped-sessions))
      (roster--start-new-session-with-directory-prompt))))

(provide 'roster)

;;; roster.el ends here
