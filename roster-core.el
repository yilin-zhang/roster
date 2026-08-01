;;; roster-core.el --- Shared foundations for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)
(require 'tabulated-list)
(require 'json)
(require 'ansi-color)

;;; Customization

(defgroup roster nil
  "Manage coding-agent sessions from Emacs."
  :group 'tools
  :prefix "roster-")

;;; Faces

(defface roster-title-face
  '((t :inherit default :weight bold))
  "Face for session titles in `roster' lists."
  :group 'roster)

(defface roster-active-face
  '((t :inherit success))
  "Face for active session state in `roster' lists."
  :group 'roster)

(defface roster-archived-face
  '((t :inherit shadow :slant italic))
  "Face for archived session state in `roster' lists."
  :group 'roster)

(defface roster-project-face
  '((t :inherit font-lock-builtin-face))
  "Face for project names in `roster' lists."
  :group 'roster)

(defface roster-directory-face
  '((t :inherit font-lock-comment-face))
  "Face for directory paths in `roster' lists."
  :group 'roster)

(defface roster-time-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for timestamps in `roster' lists."
  :group 'roster)

(defface roster-tool-opencode-face
  `((t :foreground ,(face-attribute 'ansi-color-blue :foreground)))
  "Face for the OpenCode tool tag in `roster' lists."
  :group 'roster)

(defface roster-tool-claude-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
  "Face for the Claude Code tool tag in `roster' lists."
  :group 'roster)

(defface roster-tool-codex-face
  `((t :foreground ,(face-attribute 'ansi-color-green :foreground)))
  "Face for the Codex tool tag in `roster' lists."
  :group 'roster)

(defface roster-tool-pi-face
  `((t :foreground ,(face-attribute 'ansi-color-red :foreground)))
  "Face for the pi tool tag in `roster' lists."
  :group 'roster)

(defface roster-mark-face
  '((((background dark)) (:background "DarkGoldenrod4"))
    (t (:background "LightYellow1")))
  "Face for marked rows in `roster' lists."
  :group 'roster)

(defface roster-mark-indicator-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
  "Face for the mark indicator character in `roster' lists."
  :group 'roster)

(defcustom roster-terminal-function #'roster-open-in-ghostty
  "Function used to open an external terminal and run a command.
The function is called with two args: DIRECTORY and COMMAND."
  :type 'function
  :group 'roster)

(defcustom roster-include-archived t
  "Whether `roster' shows archived sessions by default."
  :type 'boolean
  :group 'roster)

(defcustom roster-opencode-db-path
  (expand-file-name "~/.local/share/opencode/opencode.db")
  "Path to OpenCode SQLite database."
  :type 'file
  :group 'roster)

(defcustom roster-opencode-command "opencode"
  "OpenCode executable name or full path."
  :type 'string
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

(defcustom roster-pi-dir
  (expand-file-name "~/.pi/agent")
  "Path to the pi configuration directory."
  :type 'directory
  :group 'roster)

(defcustom roster-pi-command "pi"
  "The pi executable name or full path."
  :type 'string
  :group 'roster)

(defcustom roster-enabled-tools '(opencode claude codex pi)
  "List of tools whose sessions are shown by roster.
Valid elements are the symbols `opencode', `claude', `codex', and `pi'."
  :type '(set (const opencode) (const claude) (const codex) (const pi))
  :group 'roster)

(defcustom roster-default-new-session-tool 'opencode
  "Default tool when creating a new session and multiple tools are enabled.
Must be a symbol present in `roster-enabled-tools'."
  :type '(choice (const opencode) (const claude) (const codex) (const pi))
  :group 'roster)

;;; Variables

(defvar roster-buffer-name "*Roster*"
  "Buffer name used for the main `roster' session list.")

(defvar-local roster-source-function nil
  "Function returning sessions for the current `roster' list buffer.")

(defvar-local roster-show-archived roster-include-archived
  "Whether the current `roster' list buffer shows archived sessions.")

(defvar-local roster--marked (make-hash-table :test #'equal)
  "Hash table of marked session keys in the current `roster' list buffer.")

(defvar-local roster--mark-overlays (make-hash-table :test #'equal)
  "Hash table mapping session keys to overlay pairs in the current buffer.")

(defvar-local roster--sessions nil
  "Sessions represented by the current `roster' buffer contents.")

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

(defun roster--session-key (session)
  "Return the backend-qualified identity key for SESSION."
  (cons (roster--session-tool session) (roster--session-id session)))

(defun roster--session-title (session)
  "Return SESSION title with control characters collapsed to spaces."
  (replace-regexp-in-string "[[:cntrl:]]" " " (plist-get session :title)))

(defun roster--session-directory (session)
  "Return SESSION directory."
  (plist-get session :directory))

(defun roster--truncate-title (title)
  "Return TITLE truncated to `roster--title-max-length' characters."
  (if (> (length title) roster--title-max-length)
      (concat (substring title 0 (- roster--title-max-length 3)) "...")
    title))

(defun roster--sort-sessions (sessions)
  "Return SESSIONS sorted by descending update time."
  (sort sessions
        (lambda (a b)
          (> (or (plist-get a :time-updated) 0)
             (or (plist-get b :time-updated) 0)))))

(defun roster--session-archived-p (session)
  "Return non-nil when SESSION is archived."
  (numberp (plist-get session :time-archived)))

(defun roster--active-sessions (sessions)
  "Return unarchived SESSIONS."
  (seq-remove #'roster--session-archived-p sessions))

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
      'roster-archived-face
    'roster-active-face))

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

(defun roster--read-json (string)
  "Parse JSON STRING as a plist, or return nil on failure."
  (condition-case nil
      (let ((json-object-type 'plist)
            (json-key-type 'keyword)
            (json-array-type 'list)
            (json-null nil)
            (json-false nil))
        (json-read-from-string string))
    (error nil)))

(defun roster--content-text (content)
  "Return the first useful text string from message CONTENT."
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

;;; Terminal functions

(defun roster-open-in-ghostty (directory command)
  "Open Ghostty in DIRECTORY and run COMMAND in a new tab."
  (unless (eq system-type 'darwin)
    (user-error "Roster-open-in-ghostty is only available on macOS"))
  (let* ((dir (expand-file-name directory))
         (osa (or (executable-find "osascript")
                  (user-error "Osascript not found in PATH")))
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
    (user-error "Roster-open-in-iterm is only available on macOS"))
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
                  (user-error "Osascript not found in PATH")))
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

(provide 'roster-core)

;;; roster-core.el ends here
