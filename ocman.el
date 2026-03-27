;;; ocman.el --- OpenCode and Claude Code session manager for Emacs -*- lexical-binding: t; -*-

;; Author: yilinzhang
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience

;;; Commentary:

;; Manage sessions for OpenCode (via SQLite) and Claude Code (via JSONL files).
;; Both tools are shown in a unified session list tagged with "OC" or "CC".
;; Session operations — resume, rename, archive/unarchive, delete — work for
;; both tools.  Directory moves are OpenCode-only.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)
(require 'term)
(require 'tabulated-list)
(require 'json)
(require 'ansi-color)

(declare-function vterm "ext:vterm" (&optional buffer-name))
(declare-function vterm-send-string "ext:vterm" (string &optional paste-p))
(declare-function vterm-send-return "ext:vterm" ())

(defgroup ocman nil
  "OpenCode session manager."
  :group 'tools
  :prefix "ocman-")

(defface ocman-list-title-face
  '((t :inherit default :weight bold))
  "Face for session titles in `ocman' lists."
  :group 'ocman)

(defface ocman-list-active-face
  '((t :inherit success))
  "Face for active session state in `ocman' lists."
  :group 'ocman)

(defface ocman-list-archived-face
  '((t :inherit shadow :slant italic))
  "Face for archived session state in `ocman' lists."
  :group 'ocman)

(defface ocman-list-project-face
  '((t :inherit font-lock-builtin-face))
  "Face for project names in `ocman' lists."
  :group 'ocman)

(defface ocman-list-directory-face
  '((t :inherit font-lock-comment-face))
  "Face for directory paths in `ocman' lists."
  :group 'ocman)

(defface ocman-list-time-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for timestamps in `ocman' lists."
  :group 'ocman)

(defface ocman-list-tool-opencode-face
  `((t :foreground ,(face-attribute 'ansi-color-blue :foreground)))
  "Face for the OpenCode tool tag in `ocman' lists."
  :group 'ocman)

(defface ocman-list-tool-claude-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
  "Face for the Claude Code tool tag in `ocman' lists."
  :group 'ocman)

(defcustom ocman-opencode-db-path
  (expand-file-name "~/.local/share/opencode/opencode.db")
  "Path to OpenCode SQLite database."
  :type 'file
  :group 'ocman)

(defcustom ocman-opencode-command "opencode"
  "OpenCode executable name or full path."
  :type 'string
  :group 'ocman)

(defcustom ocman-terminal-function #'ocman-open-in-emacs-terminal
  "Function used to open terminal and run command.
The function is called with two args: DIRECTORY and COMMAND."
  :type 'function
  :group 'ocman)

(defcustom ocman-list-include-archived t
  "Whether `ocman-list-sessions' shows archived sessions by default."
  :type 'boolean
  :group 'ocman)

(defcustom ocman-claude-dir
  (expand-file-name "~/.claude")
  "Path to the Claude Code configuration directory."
  :type 'directory
  :group 'ocman)

(defcustom ocman-claude-command "claude"
  "Claude Code executable name or full path."
  :type 'string
  :group 'ocman)

(defcustom ocman-enabled-tools '(opencode claude)
  "List of tools whose sessions are shown by ocman.
Valid elements are the symbols `opencode' and `claude'."
  :type '(set (const opencode) (const claude))
  :group 'ocman)

(defcustom ocman-default-new-session-tool 'opencode
  "Default tool when creating a new session and multiple tools are enabled.
Must be a symbol present in `ocman-enabled-tools'."
  :type '(choice (const opencode) (const claude))
  :group 'ocman)

(defvar ocman--completion-directory-map nil
  "Alist from displayed candidate title to session directory.")

(defvar ocman-list-buffer-name "*ocman*"
  "Buffer name used for the main `ocman' session list.")

(defvar-local ocman-list-source-function nil
  "Function returning sessions for the current `ocman' list buffer.")

(defvar-local ocman-list-show-archived ocman-list-include-archived
  "Whether the current `ocman' list buffer shows archived sessions.")

(defconst ocman--sqlite-separator "	"
  "Field separator used for SQLite output.")

(defun ocman--session-tool (session)
  "Return SESSION backend symbol, defaulting to `opencode'."
  (or (plist-get session :tool) 'opencode))

(defun ocman--session-id (session)
  "Return SESSION id."
  (plist-get session :id))

(defun ocman--session-title (session)
  "Return SESSION title."
  (plist-get session :title))

(defun ocman--session-directory (session)
  "Return SESSION directory."
  (plist-get session :directory))

(defun ocman--sqlite-split-row (row)
  "Split SQLite ROW using `ocman--sqlite-separator'."
  (split-string row ocman--sqlite-separator))

(defun ocman--sqlite-last-change-p (output)
  "Return non-nil when SQLite OUTPUT reports one changed row."
  (string= (car (last (split-string output "\n" t))) "1"))

(defun ocman--sort-sessions (sessions)
  "Return SESSIONS sorted by descending update time."
  (sort sessions
        (lambda (a b)
          (> (or (plist-get a :time-updated) 0)
             (or (plist-get b :time-updated) 0)))))

(defun ocman--enabled-tool-p (tool)
  "Return non-nil when TOOL is enabled in `ocman-enabled-tools'."
  (memq tool ocman-enabled-tools))

(defun ocman--buffer-name-for-directory (directory)
  "Return an `ocman' terminal buffer name for DIRECTORY."
  (let ((name (file-name-nondirectory
               (directory-file-name (file-name-as-directory directory)))))
    (generate-new-buffer-name (format "*ocman:%s*" name))))

(defun ocman--available-shell ()
  "Return the preferred interactive shell path."
  (or explicit-shell-file-name shell-file-name (getenv "SHELL") "/bin/sh"))

(defun ocman--readable-session-list (sessions missing-message)
  "Return SESSIONS or signal MISSING-MESSAGE when empty."
  (unless sessions
    (user-error "%s" missing-message))
  sessions)

(defun ocman--select-from-sessions (sessions prompt missing-message)
  "Select one entry from SESSIONS using PROMPT.
Signal MISSING-MESSAGE when SESSIONS is empty."
  (ocman--select-session
   (ocman--readable-session-list sessions missing-message)
   prompt))

(defun ocman--claude-jsonl-files (projects-dir)
  "Return `(ENCODED-DIR . PATH)' pairs for Claude JSONL files in PROJECTS-DIR."
  (let (result)
    (dolist (encoded-dir (directory-files projects-dir nil "^[^.]"))
      (let ((dir-path (expand-file-name encoded-dir projects-dir)))
        (when (file-directory-p dir-path)
          (dolist (fname (directory-files dir-path nil "\\.jsonl\\'"))
            (push (cons encoded-dir (expand-file-name fname dir-path)) result)))))
    (nreverse result)))

(defun ocman-open-in-emacs-terminal (directory command)
  "Open terminal in Emacs for DIRECTORY and run COMMAND.
Uses `vterm' when available, otherwise falls back to `ansi-term'."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (buffer-name (ocman--buffer-name-for-directory default-directory)))
    (if (fboundp 'vterm)
        (let ((buf (vterm buffer-name)))
          (with-current-buffer buf
            (vterm-send-string command)
            (vterm-send-return))
          (pop-to-buffer buf))
      (let* ((shell (ocman--available-shell))
             (buf (ansi-term shell buffer-name)))
        (with-current-buffer buf
          (term-send-raw-string (concat command "\n")))
        (pop-to-buffer buf)))))

(defun ocman-open-in-ghostty (directory command)
  "Open Ghostty in DIRECTORY and run COMMAND in a new tab."
  (unless (eq system-type 'darwin)
    (user-error "ocman-open-in-ghostty is only available on macOS"))
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
            (format "set initial input of cfg to %s" (prin1-to-string (concat command "\n")))
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

(defun ocman-open-in-iterm (directory command)
  "Open iTerm in DIRECTORY and run COMMAND in a new tab."
  (unless (eq system-type 'darwin)
    (user-error "ocman-open-in-iterm is only available on macOS"))
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

(defun ocman--sql-quote (value)
  "Return SQL single-quoted VALUE with escaped apostrophes."
  (concat "'" (replace-regexp-in-string "'" "''" value t t) "'"))

(defun ocman--sqlite-output (sql)
  "Run SQL against the OpenCode database and return trimmed stdout."
  (unless (file-readable-p ocman-opencode-db-path)
    (user-error "OpenCode database not found: %s" ocman-opencode-db-path))
  (let ((sqlite3 (executable-find "sqlite3")))
    (unless sqlite3
      (user-error "sqlite3 command not found in PATH"))
    (with-temp-buffer
      (unless (eq 0 (call-process sqlite3 nil t nil
                                  "-separator" ocman--sqlite-separator
                                  ocman-opencode-db-path
                                  sql))
        (user-error "sqlite3 query failed: %s" (string-trim (buffer-string))))
      (string-trim-right (buffer-string)))))

(defun ocman--parse-project-row (row)
  "Return project plist parsed from SQLite ROW."
  (pcase-let ((`(,id ,worktree ,name) (ocman--sqlite-split-row row)))
    (list :id id
          :worktree (expand-file-name worktree)
          :name (unless (string-empty-p name) name))))

(defun ocman--parse-opencode-session-row (row)
  "Return OpenCode session plist parsed from SQLite ROW."
  (pcase-let ((`(,id ,title ,directory ,project-id ,time-updated ,archived-raw)
                (ocman--sqlite-split-row row)))
    (list :id id
          :title (if (string-empty-p title) "(untitled)" title)
          :directory (expand-file-name directory)
          :project-id project-id
          :time-updated (string-to-number (or time-updated "0"))
          :time-archived (unless (string-empty-p (or archived-raw ""))
                           (string-to-number archived-raw))
          :tool 'opencode)))

(defun ocman--query-projects (sql)
  "Return project plists for SQL query SQL."
  (let ((output (ocman--sqlite-output sql)))
    (unless (string-empty-p output)
      (mapcar #'ocman--parse-project-row (split-string output "\n" t)))))

(defun ocman--opencode-load-sessions ()
  "Return root OpenCode sessions as a list of plists.
Each plist has keys :id, :title, :directory, :project-id,
:time-updated, :time-archived, and :tool (always `opencode')."
  (let* ((sql (concat
                "SELECT id, title, directory, project_id, time_updated, "
                "COALESCE(time_archived, '') "
                "FROM session WHERE parent_id IS NULL ORDER BY time_updated DESC;"))
         (output (ocman--sqlite-output sql)))
    (unless (string-empty-p output)
      (mapcar #'ocman--parse-opencode-session-row
              (split-string output "\n" t)))))

(defun ocman--load-sessions ()
  "Return sessions from all enabled tools as a unified list, newest-first.
Loads from OpenCode and/or Claude Code per `ocman-enabled-tools'."
  (let (all)
    (when (ocman--enabled-tool-p 'opencode)
      (condition-case nil
          (setq all (nconc all (ocman--opencode-load-sessions)))
        (user-error nil)))
    (when (ocman--enabled-tool-p 'claude)
      (setq all (nconc all (or (ocman--claude-load-sessions) nil))))
    (ocman--sort-sessions all)))

(defun ocman--session-archived-p (session)
  "Return non-nil when SESSION is archived."
  (numberp (plist-get session :time-archived)))

(defun ocman--active-sessions (sessions)
  "Return unarchived SESSIONS."
  (seq-remove #'ocman--session-archived-p sessions))

(defun ocman--archived-sessions (sessions)
  "Return archived SESSIONS."
  (seq-filter #'ocman--session-archived-p sessions))

(defun ocman--project-for-directory (directory)
  "Return project plist for DIRECTORY when it matches a project worktree exactly."
  (let* ((dir (directory-file-name (expand-file-name directory)))
         (sql (concat
               "SELECT id, worktree, COALESCE(name, '') FROM project "
               "WHERE worktree = " (ocman--sql-quote dir) " LIMIT 1;"))
         (projects (ocman--query-projects sql)))
    (car projects)))

(defun ocman--projects-containing-directory (directory)
  "Return OpenCode projects whose worktrees contain DIRECTORY."
  (let* ((dir (directory-file-name (expand-file-name directory)))
         (sql (concat
               "SELECT id, worktree, COALESCE(name, '') FROM project "
               "WHERE worktree = " (ocman--sql-quote dir) " "
               "OR (LENGTH(" (ocman--sql-quote dir) ") > LENGTH(worktree) "
               "AND SUBSTR(" (ocman--sql-quote dir) ", 1, LENGTH(worktree) + 1) = worktree || '/') "
               "ORDER BY LENGTH(worktree) DESC;")))
    (ocman--query-projects sql)))

(defun ocman--global-project ()
  "Return the OpenCode global project plist."
  (let* ((sql (concat
               "SELECT id, worktree, COALESCE(name, '') FROM project "
               "WHERE id = 'global' LIMIT 1;")))
    (car (ocman--query-projects sql))))

(defun ocman--project-label (project)
  "Return a completion label for PROJECT."
  (let ((worktree (plist-get project :worktree))
        (name (plist-get project :name)))
    (if name
        (format "%s (%s)" worktree name)
      worktree)))

(defun ocman--resolve-target-project (directory)
  "Return the best OpenCode project for DIRECTORY.
Prefer an exact worktree match, otherwise fall back to a parent project whose
worktree contains DIRECTORY, and finally the global project."
  (or (ocman--project-for-directory directory)
      (car (ocman--projects-containing-directory directory))
      (ocman--global-project)))

(defun ocman--session-with-project-worktree (session-id)
  "Return session plist for SESSION-ID including its project worktree."
  (let* ((sql (concat
               "SELECT s.id, s.title, s.directory, s.project_id, COALESCE(p.worktree, ''), "
               "COALESCE(s.time_archived, '') "
               "FROM session s LEFT JOIN project p ON p.id = s.project_id "
               "WHERE s.id = " (ocman--sql-quote session-id) " LIMIT 1;"))
         (output (ocman--sqlite-output sql)))
    (unless (string-empty-p output)
      (pcase-let ((`(,id ,title ,directory ,project-id ,project-worktree ,archived-raw)
                    (ocman--sqlite-split-row output)))
        (list :id id
              :title (if (string-empty-p title) "(untitled)" title)
              :directory (expand-file-name directory)
              :project-id project-id
              :time-archived (unless (string-empty-p archived-raw)
                               (string-to-number archived-raw))
              :project-worktree (unless (string-empty-p project-worktree)
                                  (expand-file-name project-worktree)))))))

(defun ocman--move-session-directory (session-id directory project-id)
  "Move SESSION-ID to DIRECTORY and PROJECT-ID in one transaction."
  (let* ((dir (directory-file-name (expand-file-name directory)))
         (sql (concat
               "BEGIN IMMEDIATE;"
               "UPDATE session SET "
               "directory = " (ocman--sql-quote dir) ", "
               "project_id = " (ocman--sql-quote project-id) ", "
               "time_updated = CAST(unixepoch('subsec') * 1000 AS INTEGER) "
               "WHERE id = " (ocman--sql-quote session-id) ";"
               "SELECT changes();"
               "COMMIT;"))
         (output (ocman--sqlite-output sql)))
    (ocman--sqlite-last-change-p output)))

(defun ocman--set-session-title (session-id title)
  "Set SESSION-ID title to TITLE.
Return non-nil when one row was updated."
  (let* ((sql (concat
               "UPDATE session SET title = " (ocman--sql-quote title)
               " WHERE id = " (ocman--sql-quote session-id) ";"
               "SELECT changes();"))
         (output (ocman--sqlite-output sql)))
    (ocman--sqlite-last-change-p output)))

(defun ocman--set-session-archived (session-id archived)
  "Set SESSION-ID archived state to ARCHIVED.
Return non-nil when one row was updated."
  (let* ((value (if archived
                    (number-to-string (floor (* 1000 (float-time (current-time)))))
                  "NULL"))
         (sql (concat
               "UPDATE session SET time_archived = " value
               " WHERE id = " (ocman--sql-quote session-id) ";"
               "SELECT changes();"))
         (output (ocman--sqlite-output sql)))
    (ocman--sqlite-last-change-p output)))

(defun ocman--session-display-title (session)
  "Return display title for SESSION."
  (if (ocman--session-archived-p session)
      (format "%s [archived]" (plist-get session :title))
    (plist-get session :title)))

(defun ocman--session-state (session)
  "Return display state for SESSION."
  (if (ocman--session-archived-p session)
      "archived"
    "active"))

(defun ocman--format-time-millis (millis)
  "Format MILLIS since epoch for list display."
  (if (and millis (> millis 0))
      (format-time-string "%Y-%m-%d %H:%M"
                          (seconds-to-time (/ millis 1000.0)))
    ""))

(defun ocman--state-face (session)
  "Return the face used for SESSION state."
  (if (ocman--session-archived-p session)
      'ocman-list-archived-face
    'ocman-list-active-face))

(defun ocman--session-by-id (session-id)
  "Return root session plist for SESSION-ID, or nil when missing."
  (seq-find (lambda (session)
              (string= (plist-get session :id) session-id))
            (ocman--load-sessions)))

(defun ocman--run-command (directory command)
  "Run COMMAND in DIRECTORY and return its trimmed stdout.
Signal a `user-error' when the command exits unsuccessfully."
  (let* ((dir (expand-file-name directory))
         (default-directory
           (file-name-as-directory
            (if (file-directory-p dir)
                dir
              (expand-file-name "~")))))
    (with-temp-buffer
      (let ((status (call-process-shell-command command nil t)))
        (unless (eq status 0)
          (user-error
            "Command failed in %s: %s"
           default-directory
           (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun ocman--opencode-delete-session (session)
  "Delete OpenCode SESSION via the official CLI workflow."
  (let* ((session-id (plist-get session :id))
         (directory (plist-get session :directory))
         (command (format "%s session delete %s"
                          ocman-opencode-command
                          (shell-quote-argument session-id))))
    (ocman--run-command directory command)))

;;; Claude Code backend

(defun ocman--claude-projects-dir ()
  "Return the Claude Code projects directory."
  (expand-file-name "projects" ocman-claude-dir))

(defun ocman--claude-sidecar-path (encoded-dir session-id)
  "Return the path to the ocman sidecar JSON file for a Claude Code session."
  (expand-file-name (concat session-id ".ocman.json")
                    (expand-file-name encoded-dir (ocman--claude-projects-dir))))

(defun ocman--claude-read-sidecar (encoded-dir session-id)
  "Return ocman metadata alist for a Claude Code session, or nil if no sidecar."
  (let ((path (ocman--claude-sidecar-path encoded-dir session-id)))
    (when (file-readable-p path)
      (condition-case nil
          (let ((json-object-type 'alist)
                (json-key-type 'string))
            (json-read-file path))
        (error nil)))))

(defun ocman--claude-write-sidecar (encoded-dir session-id title time-archived)
  "Write ocman sidecar JSON for a Claude Code session.
TITLE and TIME-ARCHIVED may be nil; nil fields are omitted."
  (let ((path (ocman--claude-sidecar-path encoded-dir session-id))
        (data (append (when title `(("title" . ,title)))
                      (when time-archived `(("time_archived" . ,time-archived))))))
    (with-temp-file path
      (insert (json-encode data)))))

(defun ocman--claude-read-json (string)
  "Parse JSON STRING as a plist, or return nil on failure."
  (condition-case nil
      (let ((json-object-type 'plist)
            (json-key-type 'keyword)
            (json-array-type 'list)
            (json-null nil)
            (json-false nil))
        (json-read-from-string string))
    (error nil)))

(defun ocman--claude-content-text (content)
  "Return the first useful text string from Claude CONTENT."
  (cond
   ((and (stringp content)
         (not (string-empty-p (string-trim content))))
    (string-trim content))
   ((listp content)
    (catch 'found
      (dolist (part content)
        (when (and (listp part)
                   (equal (plist-get part :type) "text"))
          (let ((text (string-trim (or (plist-get part :text) ""))))
            (unless (string-empty-p text)
              (throw 'found text)))))
      nil))))

(defun ocman--claude-update-meta-from-object (obj slug cwd title-candidate)
  "Update Claude metadata from OBJ.
Return a plist with keys :slug, :cwd, and :title-candidate."
  (let ((new-slug (or slug (plist-get obj :slug)))
        (new-cwd cwd)
        (new-title title-candidate))
    (when (equal (plist-get obj :type) "user")
      (unless new-cwd
        (let ((value (plist-get obj :cwd)))
          (when (and (stringp value) (not (string-empty-p value)))
            (setq new-cwd value))))
      (unless new-title
        (setq new-title
              (ocman--claude-content-text
               (plist-get (plist-get obj :message) :content)))))
    (list :slug new-slug
          :cwd new-cwd
          :title-candidate new-title)))

(defun ocman--claude-title (meta sidecar)
  "Return display title derived from META and SIDECAR."
  (let ((slug (plist-get meta :slug))
        (candidate (plist-get meta :title-candidate))
        (sidecar-title (cdr (assoc "title" sidecar))))
    (or sidecar-title
        slug
        (when candidate
          (if (> (length candidate) 60)
              (concat (substring candidate 0 57) "...")
            candidate))
        "(untitled)")))

(defun ocman--claude-session-from-file (encoded-dir path)
  "Return unified Claude session plist for ENCODED-DIR and JSONL PATH."
  (let* ((session-id (file-name-sans-extension (file-name-nondirectory path)))
         (meta (ocman--claude-parse-jsonl path)))
    (when meta
      (let* ((sidecar (ocman--claude-read-sidecar encoded-dir session-id))
             (cwd (plist-get meta :cwd))
             (time-archived (let ((value (cdr (assoc "time_archived" sidecar))))
                              (when (numberp value) value))))
        (list :id session-id
              :title (ocman--claude-title meta sidecar)
              :directory (expand-file-name (if (string-empty-p cwd) "~" cwd))
              :project-id encoded-dir
              :time-updated (plist-get meta :time-updated)
              :time-archived time-archived
              :tool 'claude
              :encoded-dir encoded-dir)))))

(defun ocman--claude-parse-jsonl (path)
  "Return metadata plist from the head of a Claude Code JSONL file at PATH.
Reads up to 8 KB.  Returns plist with keys :slug, :cwd,
:title-candidate, and :time-updated (file mtime in milliseconds)."
  (condition-case nil
      (let (slug cwd title-candidate)
        (with-temp-buffer
          (insert-file-contents path nil 0 8192)
          (goto-char (point-min))
          (while (and (not (eobp))
                      (not (and slug cwd title-candidate)))
            (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                   (obj (ocman--claude-read-json line)))
              (when obj
                (pcase-let ((`(:slug ,new-slug :cwd ,new-cwd :title-candidate ,new-title)
                             (ocman--claude-update-meta-from-object obj slug cwd title-candidate)))
                  (setq slug new-slug
                        cwd new-cwd
                        title-candidate new-title))))
            (forward-line 1)))
        (let* ((attrs (file-attributes path))
               (mtime (when attrs (file-attribute-modification-time attrs)))
               (time-updated (if mtime
                                 (floor (* 1000 (float-time mtime)))
                               0)))
          (list :slug slug
                :cwd (or cwd "")
                :title-candidate title-candidate
                :time-updated time-updated)))
    (error nil)))

(defun ocman--claude-load-sessions ()
  "Return Claude Code sessions as a list of unified session plists."
  (let ((projects-dir (ocman--claude-projects-dir)))
    (when (file-directory-p projects-dir)
      (let ((sessions
             (delq nil
                   (mapcar (lambda (entry)
                             (ocman--claude-session-from-file (car entry) (cdr entry)))
                           (ocman--claude-jsonl-files projects-dir)))))
        (ocman--sort-sessions sessions)))))

(defun ocman--claude-delete-session (session)
  "Delete a Claude Code SESSION's JSONL file and ocman sidecar."
  (let* ((session-id (plist-get session :id))
         (encoded-dir (plist-get session :encoded-dir))
         (dir (expand-file-name encoded-dir (ocman--claude-projects-dir)))
         (jsonl (expand-file-name (concat session-id ".jsonl") dir))
         (sidecar (ocman--claude-sidecar-path encoded-dir session-id)))
    (when (file-exists-p jsonl)
      (delete-file jsonl))
    (when (file-exists-p sidecar)
      (delete-file sidecar))))

(defun ocman--claude-rename-session-command (session)
  "Rename a Claude Code SESSION via its ocman sidecar; return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (encoded-dir (plist-get session :encoded-dir))
         (old-title (ocman--session-title session))
         (new-title (ocman--read-session-title session)))
    (if (string= old-title new-title)
        (progn (message "Session %s already uses that title" session-id) nil)
      (let ((sidecar (ocman--claude-read-sidecar encoded-dir session-id)))
        (ocman--claude-write-sidecar
         encoded-dir session-id
         new-title
         (when sidecar (cdr (assoc "time_archived" sidecar))))
        (message "Renamed session %s to %s" session-id new-title)
        t))))

(defun ocman--claude-set-archived-command (session archived)
  "Set a Claude Code SESSION archived state to ARCHIVED; return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (encoded-dir (plist-get session :encoded-dir))
         (title (plist-get session :title))
         (verb (if archived "Archive" "Unarchive")))
    (when (yes-or-no-p (format "%s Claude session '%s' (%s)? " verb title session-id))
      (let* ((sidecar (ocman--claude-read-sidecar encoded-dir session-id))
             (old-title (when sidecar (cdr (assoc "title" sidecar))))
             (new-archived (when archived
                             (floor (* 1000 (float-time (current-time)))))))
        (ocman--claude-write-sidecar encoded-dir session-id old-title new-archived)
        (message "%sd session %s" verb session-id)
        t))))

;;; Tool helpers

(defun ocman--tool-label (session)
  "Return the short tool tag string for SESSION."
  (pcase (plist-get session :tool)
    ('claude "CC")
    (_        "OC")))

(defun ocman--tool-face (session)
  "Return the face for SESSION's tool tag."
  (pcase (ocman--session-tool session)
    ('claude 'ocman-list-tool-claude-face)
    (_        'ocman-list-tool-opencode-face)))

(defun ocman--session-command (session)
  "Return the shell command used to resume SESSION."
  (pcase (ocman--session-tool session)
    ('claude
     (format "%s -r %s"
             ocman-claude-command
             (shell-quote-argument (ocman--session-id session))))
    (_
     (format "%s -s %s"
             ocman-opencode-command
             (shell-quote-argument (ocman--session-id session))))))

(defun ocman--new-session-command (tool)
  "Return the command used to create a new TOOL session."
  (pcase tool
    ('claude ocman-claude-command)
    (_ ocman-opencode-command)))

(defun ocman--select-tool-for-new-session ()
  "Return the tool symbol to use for a new session."
  (if (cdr ocman-enabled-tools)
      (intern (completing-read
               "Tool: "
               (mapcar #'symbol-name ocman-enabled-tools)
               nil t nil nil
               (symbol-name ocman-default-new-session-tool)))
    (or (car ocman-enabled-tools) 'opencode)))

(defun ocman--session-labels (sessions)
  "Build completion labels for SESSIONS.
Labels use title only. Duplicate titles are numbered as (1), (2), (3)."
  (let ((totals (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        labels)
    (dolist (s sessions)
      (let ((title (ocman--session-display-title s)))
        (puthash title (1+ (gethash title totals 0)) totals)))
    (dolist (s sessions)
      (let* ((title (ocman--session-display-title s))
             (total (gethash title totals 0))
             (idx (1+ (gethash title seen 0)))
             (label (if (> total 1)
                        (format "%s (%d)" title idx)
                      title)))
        (puthash title idx seen)
        (push (cons label s) labels)))
    (nreverse labels)))

(defun ocman--ensure-session-title (title)
  "Return trimmed TITLE or signal a `user-error'."
  (let ((value (string-trim title)))
    (when (string-empty-p value)
      (user-error "Session title cannot be empty"))
    value))

(defun ocman--read-session-title (session)
  "Prompt for a new title for SESSION and return it.
The return value is trimmed and guaranteed non-empty."
  (ocman--ensure-session-title
   (read-string (format "Rename session (%s): " (ocman--session-title session))
                (ocman--session-title session))))

(defun ocman--session-annotation (candidate)
  "Return shadow annotation for completion CANDIDATE."
  (let ((dir (alist-get candidate ocman--completion-directory-map nil nil #'string=)))
    (when dir
      (concat " " (propertize dir 'face 'shadow)))))

(defun ocman--select-session (sessions prompt)
  "Ask user to select from SESSIONS with PROMPT.
Return the selected session plist."
  (let* ((table (ocman--session-labels sessions))
         (ocman--completion-directory-map
          (mapcar (lambda (pair)
                    (cons (car pair) (plist-get (cdr pair) :directory)))
                  table))
         (completion-extra-properties
          '(:annotation-function ocman--session-annotation))
         (choice (completing-read prompt table nil t)))
    (cdr (assoc choice table))))

(defun ocman--directory-prefix-p (dir parent)
  "Return non-nil when DIR is within PARENT."
  (string-prefix-p (file-name-as-directory (expand-file-name parent))
                   (file-name-as-directory (expand-file-name dir))))

(defun ocman--project-scope-directory ()
  "Return project root for current directory, or current directory itself.
If `default-directory' belongs to a project, return that project root;
otherwise return `default-directory'."
  (let* ((proj (project-current nil default-directory))
          (root (when proj (project-root proj))))
    (expand-file-name (or root default-directory))))

(defun ocman--project-scoped-sessions (sessions)
  "Return SESSIONS within the current project scope."
  (let ((scope (ocman--project-scope-directory)))
    (seq-filter
     (lambda (session)
       (ocman--directory-prefix-p (plist-get session :directory) scope))
     sessions)))

(defun ocman--start-new-session-with-directory-prompt ()
  "Prompt for a directory and optional tool, then start a new session."
  (let* ((dir (read-directory-name "Directory for new session: "
                                   default-directory nil t))
         (tool (ocman--select-tool-for-new-session)))
    (funcall ocman-terminal-function dir (ocman--new-session-command tool))))

(defun ocman--resume-session (session)
  "Resume SESSION, optionally jumping to its directory in Emacs first."
  (let ((directory (ocman--session-directory session)))
    (when (y-or-n-p (format "Jump to directory in Emacs first (%s)? " directory))
      (dired directory))
    (funcall ocman-terminal-function directory (ocman--session-command session))))

(defun ocman--list-sessions ()
  "Return sessions for the current `ocman' list buffer."
  (unless ocman-list-source-function
    (user-error "No session source configured for this ocman buffer"))
  (let ((sessions (funcall ocman-list-source-function)))
    (if ocman-list-show-archived
        sessions
      (ocman--active-sessions sessions))))

(defun ocman--list-entry (session)
  "Build one tabulated list entry for SESSION."
  (let ((directory (plist-get session :directory)))
    (list (plist-get session :id)
          (vector
           (concat "  " (propertize (plist-get session :title) 'face 'ocman-list-title-face))
           (propertize (ocman--tool-label session) 'face (ocman--tool-face session))
           (propertize (upcase (ocman--session-state session)) 'face (ocman--state-face session))
           (propertize (file-name-nondirectory (directory-file-name directory))
                       'face 'ocman-list-project-face)
           (propertize directory 'face 'ocman-list-directory-face)
           (propertize (ocman--format-time-millis (plist-get session :time-updated))
                       'face 'ocman-list-time-face)))))

(defun ocman--list-refresh ()
  "Refresh `tabulated-list-entries' for the current `ocman' buffer."
  (setq tabulated-list-entries
        (mapcar #'ocman--list-entry (ocman--list-sessions))))

(defun ocman--session-at-point ()
  "Return the session at point in an `ocman' list buffer."
  (let ((session-id (tabulated-list-get-id)))
    (unless session-id
      (user-error "No session on this line"))
    (or (ocman--session-by-id session-id)
        (user-error "Session %s no longer exists" session-id))))

(defun ocman-list-refresh ()
  "Refresh the current `ocman' list buffer."
  (interactive)
  (revert-buffer))

(defun ocman-list-toggle-archived ()
  "Toggle whether archived sessions are shown in the current list."
  (interactive)
  (setq ocman-list-show-archived (not ocman-list-show-archived))
  (tabulated-list-revert)
  (message "%s archived sessions"
           (if ocman-list-show-archived "Showing" "Hiding")))

(defun ocman-list-resume ()
  "Resume the session on the current line."
  (interactive)
  (ocman--resume-session (ocman--session-at-point)))

(defun ocman-list-open-directory ()
  "Open the current session's directory in Dired."
  (interactive)
  (dired (plist-get (ocman--session-at-point) :directory)))

(defun ocman-list-rename ()
  "Rename the session on the current line."
  (interactive)
  (when (ocman--rename-session-command (ocman--session-at-point))
    (tabulated-list-revert)))

(defun ocman-list-toggle-archive ()
  "Toggle archived state for the session on the current line."
  (interactive)
  (let* ((session (ocman--session-at-point))
         (archived (not (ocman--session-archived-p session))))
    (when (ocman--set-session-archived-command session archived)
      (tabulated-list-revert))))

(defun ocman-list-move-directory ()
  "Move the session on the current line to another project directory."
  (interactive)
  (when (ocman--update-session-directory-command (ocman--session-at-point))
    (tabulated-list-revert)))

(defun ocman-list-delete ()
  "Delete the session on the current line."
  (interactive)
  (let ((line (line-number-at-pos)))
    (when (ocman--delete-session-command (ocman--session-at-point))
      (tabulated-list-revert)
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (when (eobp)
        (forward-line -1)))))

(defun ocman-list-new-session ()
  "Start a new session from the list buffer."
  (interactive)
  (ocman--start-new-session-with-directory-prompt))

(defvar ocman-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ocman-list-resume)
    (define-key map (kbd "e") #'ocman-list-resume)
    (define-key map (kbd "d") #'ocman-list-delete)
    (define-key map (kbd "r") #'ocman-list-rename)
    (define-key map (kbd "a") #'ocman-list-toggle-archive)
    (define-key map (kbd "R") #'ocman-list-move-directory)
    (define-key map (kbd "o") #'ocman-list-open-directory)
    (define-key map (kbd "c") #'ocman-list-new-session)
    (define-key map (kbd "g") #'ocman-list-refresh)
    (define-key map (kbd "t") #'ocman-list-toggle-archived)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ocman-list-mode'.")

(define-derived-mode ocman-list-mode tabulated-list-mode "ocman"
  "Major mode for managing OpenCode sessions."
  (setq tabulated-list-format [("Title"     28 t)
                               ("Tool"       4 t)
                               ("State"     10 t)
                               ("Project"   18 t)
                               ("Directory" 38 t)
                               ("Updated"   16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'ocman--list-refresh nil t)
  (tabulated-list-init-header))

(defun ocman--open-list-buffer (buffer-name source-function &optional include-archived)
  "Open an `ocman' list BUFFER-NAME using SOURCE-FUNCTION.
When INCLUDE-ARCHIVED is non-nil, archived sessions are shown initially.
When omitted or nil, the value of `ocman-list-include-archived' is used."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (ocman-list-mode)
      (setq-local ocman-list-source-function source-function)
      (setq-local ocman-list-show-archived (if (null include-archived)
                                               ocman-list-include-archived
                                             include-archived))
      (setq-local header-line-format
                  "RET/e resume, d delete, r rename, a archive, R move, o dired, c create, t archived, g refresh")
      (tabulated-list-revert))
    (pop-to-buffer buffer)))

(defun ocman--opencode-rename-session-command (session)
  "Rename an OpenCode SESSION via SQLite and return non-nil when changed."
  (let* ((session-id (plist-get session :id))
         (old-title (ocman--session-title session))
         (new-title (ocman--read-session-title session)))
    (if (string= old-title new-title)
        (progn
          (message "Session %s already uses that title" session-id)
          nil)
      (unless (ocman--set-session-title session-id new-title)
        (user-error "No session updated for id %s" session-id))
      (let ((updated (ocman--session-with-project-worktree session-id)))
        (unless (and updated
                     (string= (plist-get updated :title) new-title))
          (user-error "Session %s failed title verification" session-id))
        (message "Renamed session %s to %s" session-id new-title)
        t))))

(defun ocman--rename-session-command (session)
  "Rename SESSION and return non-nil when the title changes."
  (pcase (plist-get session :tool)
    ('claude (ocman--claude-rename-session-command session))
    (_       (ocman--opencode-rename-session-command session))))

(defun ocman--opencode-set-archived-command (session archived)
  "Set an OpenCode SESSION archived state to ARCHIVED; return non-nil on change."
  (let* ((session-id (ocman--session-id session))
         (title (ocman--session-title session))
         (verb (if archived "Archive" "Unarchive")))
    (when (yes-or-no-p (format "%s OpenCode session '%s' (%s)? " verb title session-id))
      (unless (ocman--set-session-archived session-id archived)
        (user-error "No session updated for id %s" session-id))
      (let ((updated (ocman--session-with-project-worktree session-id)))
        (unless (and updated
                     (eq (ocman--session-archived-p updated) archived))
          (user-error "Session %s failed %s verification" session-id (downcase verb)))
        (message "%sd session %s" verb session-id)
        t))))

(defun ocman--set-session-archived-command (session archived)
  "Set SESSION archived state to ARCHIVED and return non-nil on change."
  (pcase (plist-get session :tool)
    ('claude (ocman--claude-set-archived-command session archived))
    (_       (ocman--opencode-set-archived-command session archived))))

(defun ocman--delete-session-command (session)
  "Delete SESSION and return non-nil on success."
  (let ((session-id (ocman--session-id session))
        (title (ocman--session-title session))
        (directory (ocman--session-directory session)))
    (when (yes-or-no-p (format "Delete session '%s' (%s)? " title session-id))
      (pcase (plist-get session :tool)
        ('claude (ocman--claude-delete-session session))
        (_       (ocman--opencode-delete-session session)))
      (message "Deleted session %s from %s" session-id directory)
      t)))

(defun ocman--verify-moved-session (session-id directory project-id project-worktree)
  "Signal when SESSION-ID does not match DIRECTORY and PROJECT-ID.
PROJECT-WORKTREE is the expected resolved worktree for PROJECT-ID."
  (let ((updated (ocman--session-with-project-worktree session-id)))
    (unless updated
      (user-error "Updated session %s could not be reloaded" session-id))
    (unless (and (string= (plist-get updated :directory) directory)
                 (string= (plist-get updated :project-id) project-id)
                 (string= (or (plist-get updated :project-worktree) "") project-worktree))
      (user-error "Session %s failed post-update consistency checks" session-id))))

(defun ocman--target-project-for-directory (directory)
  "Return the resolved OpenCode project for DIRECTORY or signal a `user-error'."
  (or (ocman--resolve-target-project directory)
      (user-error
       (concat
        "No OpenCode project matches %s. OpenCode only stays consistent when the target "
        "directory already exists as a project worktree.")
       directory)))

(defun ocman--move-session-confirmed-p (session-id old-dir new-dir)
  "Return non-nil when the user confirms moving SESSION-ID from OLD-DIR to NEW-DIR."
  (yes-or-no-p (format "Move session %s from %s to %s? " session-id old-dir new-dir)))

(defun ocman--update-session-directory-command (session)
  "Move SESSION to another known OpenCode project directory.
Signals a `user-error' for Claude Code sessions, which are not movable."
  (when (eq (plist-get session :tool) 'claude)
    (user-error "Directory moves are not supported for Claude Code sessions"))
  (let* ((session-id (plist-get session :id))
         (old-dir (plist-get session :directory))
         (old-project-id (plist-get session :project-id))
         (new-dir (directory-file-name
                   (expand-file-name
                     (read-directory-name (format "New directory (current: %s): " old-dir)
                                          old-dir nil t)))))
    (unless (file-directory-p new-dir)
      (user-error "Directory does not exist: %s" new-dir))
    (let* ((target-project (ocman--target-project-for-directory new-dir))
           (new-project-id (plist-get target-project :id)))
      (cond
       ((and (string= old-dir new-dir)
             (string= old-project-id new-project-id))
        (message "Session %s already points to %s" session-id new-dir)
        nil)
       ((not (ocman--move-session-confirmed-p session-id old-dir new-dir))
        nil)
       (t
        (unless (ocman--move-session-directory session-id new-dir new-project-id)
          (user-error "No session updated for id %s" session-id))
        (ocman--verify-moved-session session-id new-dir new-project-id
                                     (plist-get target-project :worktree))
        (message
         "Moved session %s to %s. Restart active OpenCode views if they still show stale state."
         session-id new-dir)
        t)))))

;;;###autoload
(defun ocman-update-session-directory ()
  "Safely move a session to another known OpenCode project directory.

This updates both `session.directory' and `session.project_id', and only
allows targets that already exist in the OpenCode `project' table."
  (interactive)
  (ocman--update-session-directory-command
   (ocman--select-from-sessions
    (ocman--load-sessions)
    "Select OpenCode session to move: "
    "No OpenCode sessions found")))

;;;###autoload
(defun ocman-rename-session ()
  "Rename an existing OpenCode session."
  (interactive)
  (ocman--rename-session-command
   (ocman--select-from-sessions
    (ocman--active-sessions (ocman--load-sessions))
    "Select OpenCode session to rename: "
    "No active OpenCode sessions found")))

;;;###autoload
(defun ocman-archive-session ()
  "Archive an active OpenCode session."
  (interactive)
  (ocman--set-session-archived-command
   (ocman--select-from-sessions
    (ocman--active-sessions (ocman--load-sessions))
    "Select OpenCode session to archive: "
    "No active OpenCode sessions found")
   t))

;;;###autoload
(defun ocman-unarchive-session ()
  "Unarchive an existing OpenCode session."
  (interactive)
  (ocman--set-session-archived-command
   (ocman--select-from-sessions
    (ocman--archived-sessions (ocman--load-sessions))
    "Select OpenCode session to unarchive: "
    "No archived OpenCode sessions found")
   nil))

;;;###autoload
(defun ocman-delete-session ()
  "Delete an existing OpenCode session using the official CLI command."
  (interactive)
  (ocman--delete-session-command
   (ocman--select-from-sessions
    (ocman--load-sessions)
    "Select OpenCode session to delete: "
    "No OpenCode sessions found")))

;;;###autoload
(defun ocman-list-sessions ()
  "Open a Dired-like buffer for managing OpenCode sessions."
  (interactive)
  (ocman--open-list-buffer ocman-list-buffer-name #'ocman--load-sessions))

;;;###autoload
(defun ocman-list-project-sessions ()
  "Open a Dired-like buffer for sessions in the current project scope."
  (interactive)
  (let ((scope (ocman--project-scope-directory)))
    (ocman--open-list-buffer
     (format "%s<%s>" ocman-list-buffer-name (file-name-nondirectory (directory-file-name scope)))
     (lambda ()
       (ocman--project-scoped-sessions (ocman--load-sessions))))))

;;;###autoload
(defun ocman-open-session ()
  "Choose from all existing OpenCode sessions and resume one.
If there are no sessions, prompt for a directory and start a new one."
  (interactive)
  (let ((sessions (ocman--load-sessions)))
    (if sessions
        (ocman--resume-session
         (ocman--select-session sessions "Select OpenCode session to resume: "))
      (ocman--start-new-session-with-directory-prompt))))

;;;###autoload
(defun ocman-open-session-project ()
  "Choose and resume OpenCode session under current project scope.
If current directory belongs to a project, scope is project root and subdirs.
Otherwise scope is current directory and subdirs.
If no matching session exists, prompt for a directory and start a new one."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (scope (ocman--project-scope-directory))
         (scoped-sessions (ocman--project-scoped-sessions sessions)))
    (if scoped-sessions
        (ocman--resume-session
         (ocman--select-session scoped-sessions
                                 (format "Select session in project scope (%s): " scope)))
      (ocman--start-new-session-with-directory-prompt))))

;;;###autoload
(defun ocman-open-latest-session-project ()
  "Resume the latest active OpenCode session in the current project scope.
If none exists, prompt for a directory and start a new one."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (scoped-sessions (ocman--active-sessions
                           (ocman--project-scoped-sessions sessions))))
    (if scoped-sessions
        (ocman--resume-session (car scoped-sessions))
      (ocman--start-new-session-with-directory-prompt))))

(define-obsolete-function-alias 'ocman-open-session-here
  'ocman-open-session-project "0.1.1")

(provide 'ocman)

;;; ocman.el ends here
