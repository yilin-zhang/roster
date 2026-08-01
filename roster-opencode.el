;;; roster-opencode.el --- OpenCode backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-core)
(require 'sqlite)

;;; OpenCode backend

(defface roster-tool-opencode-face
  `((t :foreground ,(face-attribute 'ansi-color-blue :foreground)))
  "Face for the OpenCode tool tag in `roster' lists."
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

(defun roster--opencode-load-sessions (&optional include-archived)
  "Return root OpenCode sessions as a list of plists.
Each plist has keys :id, :title, :directory, :project-id,
:time-updated, :time-archived, and :tool (always `opencode')."
  (mapcar #'roster--opencode-parse-session-row
          (roster--opencode-sqlite-rows
           (concat "SELECT id, title, directory, project_id, time_updated, "
                   "COALESCE(time_archived, '') "
                   "FROM session WHERE parent_id IS NULL "
                   (unless include-archived "AND time_archived IS NULL ")
                   "ORDER BY time_updated DESC;"))))

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

(defun roster--opencode-resume-command (session)
  "Return the shell command used to resume OpenCode SESSION."
  (format "%s -s %s" roster-opencode-command
          (shell-quote-argument (roster--session-id session))))

(defun roster--opencode-new-command ()
  "Return the shell command used to start an OpenCode session."
  roster-opencode-command)

(defun roster--opencode-rename-session (session new-title)
  "Rename OpenCode SESSION to NEW-TITLE and verify the update."
  (let ((session-id (roster--session-id session)))
    (unless (roster--opencode-set-session-title session-id new-title)
      (user-error "No session updated for id %s" session-id))
    (let ((updated (roster--opencode-session-with-project-worktree session-id)))
      (unless (and updated (string= (plist-get updated :title) new-title))
        (user-error "Session %s failed title verification" session-id)))))

(defun roster--opencode-archive-session (session archived)
  "Set OpenCode SESSION archived state to ARCHIVED and verify the update."
  (let ((session-id (roster--session-id session)))
    (unless (roster--opencode-set-session-archived session-id archived)
      (user-error "No session updated for id %s" session-id))
    (let ((updated (roster--opencode-session-with-project-worktree session-id)))
      (unless (and updated (eq (roster--session-archived-p updated) archived))
        (user-error "Session %s failed archive verification" session-id)))))

(defun roster--opencode-verify-moved-session
    (session-id directory project-id project-worktree)
  "Signal when SESSION-ID does not match DIRECTORY and PROJECT-ID.
PROJECT-WORKTREE is the expected resolved worktree for PROJECT-ID."
  (let ((updated (roster--opencode-session-with-project-worktree session-id)))
    (unless updated
      (user-error "Updated session %s could not be reloaded" session-id))
    (unless (and (string= (plist-get updated :directory) directory)
                 (string= (plist-get updated :project-id) project-id)
                 (string= (or (plist-get updated :project-worktree) "")
                          project-worktree))
      (user-error "Session %s failed post-update consistency checks" session-id))))

(defun roster--opencode-target-project-for-directory (directory)
  "Return the resolved OpenCode project for DIRECTORY or signal an error."
  (or (roster--opencode-resolve-target-project directory)
      (user-error
       (concat
        "No OpenCode project matches %s. OpenCode only stays consistent when "
        "the target directory already exists as a project worktree.")
       directory)))

(defun roster--opencode-move-session (session)
  "Interactively move OpenCode SESSION to another known project directory."
  (let* ((session-id (roster--session-id session))
         (old-dir (roster--session-directory session))
         (old-project-id (plist-get session :project-id))
         (new-dir (directory-file-name
                   (expand-file-name
                    (read-directory-name
                     (format "New directory (current: %s): " old-dir)
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
       ((not (yes-or-no-p
              (format "Move session %s from %s to %s? "
                      session-id old-dir new-dir)))
        nil)
       (t
        (unless (roster--opencode-move-session-directory
                 session-id new-dir new-project-id)
          (user-error "No session updated for id %s" session-id))
        (roster--opencode-verify-moved-session
         session-id new-dir new-project-id (plist-get target-project :worktree))
        (message
         "Moved session %s to %s. Restart active OpenCode views if state is stale."
         session-id new-dir)
        t)))))

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
  :move #'roster--opencode-move-session))

(provide 'roster-opencode)

;;; roster-opencode.el ends here
