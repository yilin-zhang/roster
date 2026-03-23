;;; ocman.el --- OpenCode session manager for Emacs -*- lexical-binding: t; -*-

;; Author: yilinzhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience

;;; Commentary:

;; Quickly resume existing OpenCode sessions from ~/.local/share/opencode/opencode.db,
;; or start a new session in a selected directory.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)
(require 'term)
(require 'tabulated-list)

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

(defvar ocman--completion-directory-map nil
  "Alist from displayed candidate title to session directory.")

(defvar ocman-list-buffer-name "*ocman*"
  "Buffer name used for the main `ocman' session list.")

(defvar-local ocman-list-source-function nil
  "Function returning sessions for the current `ocman' list buffer.")

(defvar-local ocman-list-show-archived ocman-list-include-archived
  "Whether the current `ocman' list buffer shows archived sessions.")

(defun ocman-open-in-emacs-terminal (directory command)
  "Open terminal in Emacs for DIRECTORY and run COMMAND.
Uses `vterm' when available, otherwise falls back to `ansi-term'."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (name (file-name-nondirectory (directory-file-name default-directory)))
         (buffer-name (generate-new-buffer-name (format "*ocman:%s*" name))))
    (if (fboundp 'vterm)
        (let ((buf (vterm buffer-name)))
          (with-current-buffer buf
            (vterm-send-string command)
            (vterm-send-return))
          (pop-to-buffer buf))
      (let* ((shell (or explicit-shell-file-name shell-file-name (getenv "SHELL") "/bin/sh"))
             (buf (ansi-term shell buffer-name)))
        (with-current-buffer buf
          (term-send-raw-string (concat command "\n")))
        (pop-to-buffer buf)))))

(defun ocman-open-in-ghostty (directory command)
  "Open Ghostty in DIRECTORY and run COMMAND."
  (unless (eq system-type 'darwin)
    (user-error "ocman-open-in-ghostty is only available on macOS"))
  (let* ((dir (expand-file-name directory))
         (shell (or (getenv "SHELL") "/bin/zsh"))
         (shell-command-string (format "cd %s && %s; exec %s -l"
                                       (shell-quote-argument dir)
                                       command
                                       (shell-quote-argument shell))))
    (call-process "open" nil 0 nil
                  "-a" "Ghostty" "--args"
                  "-e" shell "-lc" shell-command-string)))

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
    (let* ((cmd (format "%s %s %s"
                        (shell-quote-argument sqlite3)
                        (shell-quote-argument ocman-opencode-db-path)
                        (shell-quote-argument sql)))
           (output (string-trim-right (shell-command-to-string cmd))))
      output)))

(defun ocman--load-sessions ()
  "Return root OpenCode sessions as a list of plists.
Each plist has keys :id, :title, :directory, :project-id,
:time-updated and :time-archived."
  (let* ((sql (concat
                "SELECT id, title, directory, project_id, time_updated, "
                "COALESCE(time_archived, '') "
               "FROM session WHERE parent_id IS NULL ORDER BY time_updated DESC;"))
         (output (ocman--sqlite-output sql)))
    (if (string-empty-p output)
        nil
      (mapcar
       (lambda (line)
         (let* ((cols (split-string line "|"))
                (id (car cols))
                (title (cadr cols))
                (project-id (nth (- (length cols) 3) cols))
                (time-updated (string-to-number (nth (- (length cols) 2) cols)))
                (archived-raw (car (last cols)))
                (directory (mapconcat #'identity (cddr (butlast cols 3)) "|")))
           (list :id id
                 :title (if (string-empty-p title) "(untitled)" title)
                 :directory (expand-file-name directory)
                 :project-id project-id
                 :time-updated time-updated
                 :time-archived (unless (string-empty-p archived-raw)
                                  (string-to-number archived-raw)))))
       (split-string output "\n" t)))))

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
         (output (ocman--sqlite-output sql)))
    (unless (string-empty-p output)
      (let* ((cols (split-string output "|"))
             (id (nth 0 cols))
             (worktree (nth 1 cols))
             (name (nth 2 cols)))
        (list :id id
              :worktree (expand-file-name worktree)
              :name (unless (string-empty-p name) name))))))

(defun ocman--session-with-project-worktree (session-id)
  "Return session plist for SESSION-ID including its project worktree."
  (let* ((sql (concat
               "SELECT s.id, s.title, s.directory, s.project_id, COALESCE(p.worktree, ''), "
               "COALESCE(s.time_archived, '') "
               "FROM session s LEFT JOIN project p ON p.id = s.project_id "
               "WHERE s.id = " (ocman--sql-quote session-id) " LIMIT 1;"))
         (output (ocman--sqlite-output sql)))
    (unless (string-empty-p output)
      (let* ((cols (split-string output "|"))
             (id (nth 0 cols))
             (title (nth 1 cols))
             (project-id (nth 3 cols))
             (project-worktree (nth 4 cols))
             (archived-raw (nth 5 cols))
             (directory (nth 2 cols)))
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
         (output (ocman--sqlite-output sql))
         (lines (split-string output "\n" t)))
    (string= (car (last lines)) "1")))

(defun ocman--set-session-title (session-id title)
  "Set SESSION-ID title to TITLE.
Return non-nil when one row was updated."
  (let* ((sql (concat
               "UPDATE session SET title = " (ocman--sql-quote title)
               " WHERE id = " (ocman--sql-quote session-id) ";"
               "SELECT changes();"))
         (output (ocman--sqlite-output sql)))
    (string= (car (last (split-string output "\n" t))) "1")))

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
    (string= (car (last (split-string output "\n" t))) "1")))

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
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    (with-temp-buffer
      (let ((status (call-process-shell-command command nil t)))
        (unless (eq status 0)
          (user-error
           "Command failed in %s: %s"
           default-directory
           (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun ocman--delete-session (session)
  "Delete OpenCode SESSION via the official CLI workflow."
  (let* ((session-id (plist-get session :id))
         (directory (plist-get session :directory))
         (command (format "%s session delete %s"
                          ocman-opencode-command
                          (shell-quote-argument session-id))))
    (ocman--run-command directory command)))

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
  "Prompt for directory and start a new OpenCode session there."
  (let ((dir (read-directory-name "No session found, choose directory for new OpenCode session: "
                                  default-directory nil t)))
    (funcall ocman-terminal-function dir ocman-opencode-command)))

(defun ocman--resume-session (session)
  "Resume OpenCode SESSION, optionally jumping to its directory first."
  (let* ((session-id (plist-get session :id))
         (directory (plist-get session :directory)))
    (when (y-or-n-p (format "Jump to directory in Emacs first (%s)? " directory))
      (dired directory))
    (funcall ocman-terminal-function
     directory
     (format "%s -s %s"
             ocman-opencode-command
             (shell-quote-argument session-id)))))

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
  "Start a new OpenCode session from the list buffer."
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
  (setq tabulated-list-format [("Title" 28 t)
                               ("State" 10 t)
                               ("Project" 18 t)
                               ("Directory" 42 t)
                               ("Updated" 16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Updated" . nil))
  (add-hook 'tabulated-list-revert-hook #'ocman--list-refresh nil t)
  (tabulated-list-init-header))

(defun ocman--open-list-buffer (buffer-name source-function &optional include-archived)
  "Open an `ocman' list BUFFER-NAME using SOURCE-FUNCTION.
When INCLUDE-ARCHIVED is nil, archived sessions are hidden initially."
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

(defun ocman--rename-session-command (session)
  "Rename SESSION and return non-nil when it changes."
  (let* ((session-id (plist-get session :id))
         (old-title (plist-get session :title))
         (new-title (string-trim
                     (read-string (format "Rename session (%s): " old-title)
                                  old-title))))
    (when (string-empty-p new-title)
      (user-error "Session title cannot be empty"))
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

(defun ocman--set-session-archived-command (session archived)
  "Set SESSION archived state to ARCHIVED and return non-nil on change."
  (let* ((session-id (plist-get session :id))
         (title (plist-get session :title))
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

(defun ocman--delete-session-command (session)
  "Delete SESSION using the official CLI and return non-nil on success."
  (let ((session-id (plist-get session :id))
        (title (plist-get session :title))
        (directory (plist-get session :directory)))
    (when (yes-or-no-p (format "Delete OpenCode session '%s' (%s)? " title session-id))
      (ocman--delete-session session)
      (message "Deleted session %s from %s" session-id directory)
      t)))

(defun ocman--update-session-directory-command (session)
  "Move SESSION to another known OpenCode project directory."
  (let* ((session-id (plist-get session :id))
         (old-dir (plist-get session :directory))
         (old-project-id (plist-get session :project-id))
         (new-dir (directory-file-name
                   (expand-file-name
                    (read-directory-name (format "New directory (current: %s): " old-dir)
                                         old-dir nil t)))))
    (unless (file-directory-p new-dir)
      (user-error "Directory does not exist: %s" new-dir))
    (let ((target-project (ocman--project-for-directory new-dir)))
      (unless target-project
        (user-error
         (concat
          "No OpenCode project matches %s. OpenCode only stays consistent when the target "
          "directory already exists as a project worktree.")
         new-dir))
      (let ((new-project-id (plist-get target-project :id)))
        (if (and (string= old-dir new-dir)
                 (string= old-project-id new-project-id))
            (progn
              (message "Session %s already points to %s" session-id new-dir)
              nil)
          (when (yes-or-no-p
                 (format "Move session %s from %s to %s? " session-id old-dir new-dir))
            (unless (ocman--move-session-directory session-id new-dir new-project-id)
              (user-error "No session updated for id %s" session-id))
            (let ((updated (ocman--session-with-project-worktree session-id)))
              (unless updated
                (user-error "Updated session %s could not be reloaded" session-id))
              (unless (and (string= (plist-get updated :directory) new-dir)
                           (string= (plist-get updated :project-id) new-project-id)
                           (string= (or (plist-get updated :project-worktree) "") new-dir))
                (user-error "Session %s failed post-update consistency checks" session-id))
              (message
               "Moved session %s to %s. Restart active OpenCode views if they still show stale state."
               session-id new-dir)
              t)))))))

;;;###autoload
(defun ocman-update-session-directory ()
  "Safely move a session to another known OpenCode project directory.

This updates both `session.directory' and `session.project_id', and only
allows targets that already exist in the OpenCode `project' table."
  (interactive)
  (let ((sessions (ocman--load-sessions)))
    (unless sessions
      (user-error "No OpenCode sessions found"))
    (ocman--update-session-directory-command
     (ocman--select-session sessions "Select OpenCode session to move: "))))

;;;###autoload
(defun ocman-rename-session ()
  "Rename an existing OpenCode session."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (active-sessions (ocman--active-sessions sessions)))
    (unless active-sessions
      (user-error "No active OpenCode sessions found"))
    (ocman--rename-session-command
     (ocman--select-session active-sessions "Select OpenCode session to rename: "))))

;;;###autoload
(defun ocman-archive-session ()
  "Archive an active OpenCode session."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (active-sessions (ocman--active-sessions sessions)))
    (unless active-sessions
      (user-error "No active OpenCode sessions found"))
    (ocman--set-session-archived-command
     (ocman--select-session active-sessions "Select OpenCode session to archive: ")
     t)))

;;;###autoload
(defun ocman-unarchive-session ()
  "Unarchive an existing OpenCode session."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (archived-sessions (ocman--archived-sessions sessions)))
    (unless archived-sessions
      (user-error "No archived OpenCode sessions found"))
    (ocman--set-session-archived-command
     (ocman--select-session archived-sessions "Select OpenCode session to unarchive: ")
     nil)))

;;;###autoload
(defun ocman-delete-session ()
  "Delete an existing OpenCode session using the official CLI command."
  (interactive)
  (let ((sessions (ocman--load-sessions)))
    (unless sessions
      (user-error "No OpenCode sessions found"))
    (ocman--delete-session-command
     (ocman--select-session sessions "Select OpenCode session to delete: "))))

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
