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

(defgroup ocman nil
  "OpenCode session manager."
  :group 'tools
  :prefix "ocman-")

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

(defvar ocman--completion-directory-map nil
  "Alist from displayed candidate title to session directory.")

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

(defun ocman-open-in-iterm (directory command)
  "Open iTerm in DIRECTORY and run COMMAND."
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
     (format
      "%s -e %s"
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
  "Return all OpenCode sessions as a list of plists.
Each plist has keys :id, :title, :directory, :project-id,
:time-updated and :time-archived."
  (let* ((sql (concat
               "SELECT id, title, directory, project_id, time_updated, "
               "COALESCE(time_archived, '') "
               "FROM session ORDER BY time_updated DESC;"))
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

;;;###autoload
(defun ocman-update-session-directory ()
  "Safely move a session to another known OpenCode project directory.

This updates both `session.directory' and `session.project_id', and only
allows targets that already exist in the OpenCode `project' table."
  (interactive)
  (let ((sessions (ocman--load-sessions)))
    (unless sessions
      (user-error "No OpenCode sessions found"))
    (let* ((session (ocman--select-session sessions "Select OpenCode session to move: "))
           (session-id (plist-get session :id))
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
              (message "Session %s already points to %s" session-id new-dir)
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
                 session-id new-dir)))))))))

;;;###autoload
(defun ocman-rename-session ()
  "Rename an existing OpenCode session."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (active-sessions (ocman--active-sessions sessions)))
    (unless active-sessions
      (user-error "No active OpenCode sessions found"))
    (let* ((session (ocman--select-session active-sessions "Select OpenCode session to rename: "))
           (session-id (plist-get session :id))
           (old-title (plist-get session :title))
           (new-title (string-trim
                       (read-string (format "Rename session (%s): " old-title)
                                    old-title))))
      (when (string-empty-p new-title)
        (user-error "Session title cannot be empty"))
      (if (string= old-title new-title)
          (message "Session %s already uses that title" session-id)
        (unless (ocman--set-session-title session-id new-title)
          (user-error "No session updated for id %s" session-id))
        (let ((updated (ocman--session-with-project-worktree session-id)))
          (unless (and updated
                       (string= (plist-get updated :title) new-title))
            (user-error "Session %s failed title verification" session-id))
          (message "Renamed session %s to %s" session-id new-title))))))

;;;###autoload
(defun ocman-archive-session ()
  "Archive an active OpenCode session."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (active-sessions (ocman--active-sessions sessions)))
    (unless active-sessions
      (user-error "No active OpenCode sessions found"))
    (let* ((session (ocman--select-session active-sessions "Select OpenCode session to archive: "))
           (session-id (plist-get session :id))
           (title (plist-get session :title)))
      (when (yes-or-no-p (format "Archive OpenCode session '%s' (%s)? " title session-id))
        (unless (ocman--set-session-archived session-id t)
          (user-error "No session updated for id %s" session-id))
        (let ((updated (ocman--session-with-project-worktree session-id)))
          (unless (and updated (ocman--session-archived-p updated))
            (user-error "Session %s failed archive verification" session-id))
          (message "Archived session %s" session-id))))))

;;;###autoload
(defun ocman-unarchive-session ()
  "Unarchive an existing OpenCode session."
  (interactive)
  (let* ((sessions (ocman--load-sessions))
         (archived-sessions (ocman--archived-sessions sessions)))
    (unless archived-sessions
      (user-error "No archived OpenCode sessions found"))
    (let* ((session (ocman--select-session archived-sessions "Select OpenCode session to unarchive: "))
           (session-id (plist-get session :id))
           (title (plist-get session :title)))
      (when (yes-or-no-p (format "Unarchive OpenCode session '%s' (%s)? " title session-id))
        (unless (ocman--set-session-archived session-id nil)
          (user-error "No session updated for id %s" session-id))
        (let ((updated (ocman--session-with-project-worktree session-id)))
          (unless (and updated (not (ocman--session-archived-p updated)))
            (user-error "Session %s failed unarchive verification" session-id))
          (message "Unarchived session %s" session-id))))))

;;;###autoload
(defun ocman-delete-session ()
  "Delete an existing OpenCode session using the official CLI command."
  (interactive)
  (let ((sessions (ocman--load-sessions)))
    (unless sessions
      (user-error "No OpenCode sessions found"))
    (let* ((session (ocman--select-session sessions "Select OpenCode session to delete: "))
           (session-id (plist-get session :id))
           (title (plist-get session :title))
           (directory (plist-get session :directory)))
      (when (yes-or-no-p (format "Delete OpenCode session '%s' (%s)? " title session-id))
        (ocman--delete-session session)
        (message "Deleted session %s from %s" session-id directory)))))

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
         (scope (ocman--project-scope-directory))
         (scoped-sessions (ocman--active-sessions
                           (ocman--project-scoped-sessions sessions))))
    (if scoped-sessions
        (ocman--resume-session (car scoped-sessions))
      (ocman--start-new-session-with-directory-prompt))))

(define-obsolete-function-alias 'ocman-open-session-here
  'ocman-open-session-project "0.1.1")

(provide 'ocman)

;;; ocman.el ends here
