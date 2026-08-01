;;; roster.el --- Session manager for coding agents -*- lexical-binding: t; -*-

;; Author: Yilin Zhang
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/yilin-zhang/roster

;;; Commentary:

;; Roster manages AI coding sessions for OpenCode, Claude Code, Codex, and pi inside Emacs.
;;
;; Sessions from all tools are shown in a unified `tabulated-list-mode' buffer
;; tagged "OC" (OpenCode), "CC" (Claude Code), "CX" (Codex), or "PI" (pi).  Supported operations:
;;   resume, rename, archive/unarchive, delete, and directory moves (OpenCode only).
;;
;; Storage backends:
;;   OpenCode  — reads and writes the SQLite database at `roster-opencode-db-path'.
;;   Claude Code — reads JSONL conversation files under `roster-claude-dir'/projects/.
;;                 Custom metadata (title, archive state) is kept in .roster.json
;;                 sidecar files because Claude Code's database is not third-party
;;                 writable.
;;   Codex     — lists, renames, archives, and deletes threads through the official
;;                 app-server API.  Deprecated .roster.json sidecars remain
;;                 read-only title fallbacks during migration.
;;   pi        — reads JSONL files under `roster-pi-dir'/sessions/.  Custom metadata
;;                 (archive state) is kept in .roster.json sidecar files under
;;                 `roster-pi-dir'/roster/.  Renames append `session_info' entries so
;;                 pi itself sees the updated display name.
;;
;; Requires Emacs 29.1+ for built-in SQLite support (sqlite.el).

;;; Code:

(require 'roster-core)
(require 'roster-opencode)
(require 'roster-claude)
(require 'roster-codex)
(require 'roster-pi)

;;; Tool helpers

(defun roster--load-sessions ()
  "Return sessions from all enabled tools as a unified list, newest-first."
  (let ((loaders `((opencode . ,#'roster--opencode-load-sessions)
                   (claude   . ,#'roster--claude-load-sessions)
                   (codex    . ,#'roster--codex-load-sessions)
                   (pi       . ,#'roster--pi-load-sessions))))
    (roster--sort-sessions
     (seq-mapcat
      (lambda (tool)
        (condition-case err
            (funcall (alist-get tool loaders) roster-show-archived)
          (error (message "roster: %s sessions unavailable: %s"
                          tool (error-message-string err))
                 nil)))
      roster-enabled-tools))))

(defun roster--tool-label (session)
  "Return the short tool tag string for SESSION."
  (pcase (roster--session-tool session)
    ('claude "CC")
    ('codex  "CX")
    ('pi     "PI")
    (_       "OC")))

(defun roster--tool-face (session)
  "Return the face for SESSION's tool tag."
  (pcase (roster--session-tool session)
    ('claude 'roster-tool-claude-face)
    ('codex  'roster-tool-codex-face)
    ('pi     'roster-tool-pi-face)
    (_       'roster-tool-opencode-face)))

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
    ('pi
     (format "%s --session %s"
             roster-pi-command
             (shell-quote-argument (plist-get session :file-path))))
    (_
     (format "%s -s %s"
             roster-opencode-command
             (shell-quote-argument (roster--session-id session))))))

(defun roster--new-session-command (tool)
  "Return the command used to create a new TOOL session."
  (pcase tool
    ('claude roster-claude-command)
    ('codex  roster-codex-command)
    ('pi     roster-pi-command)
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

;;; Mode internals

(defun roster--session-by-key (session-key)
  "Return the cached session for backend-qualified SESSION-KEY."
  (when session-key
    (seq-find (lambda (session)
                (equal (roster--session-key session) session-key))
              roster--sessions)))

(defun roster--visible-sessions ()
  "Return sessions for the current `roster' list buffer."
  (unless roster-source-function
    (user-error "No session source configured for this roster buffer"))
  (let ((sessions (funcall roster-source-function)))
    (if roster-show-archived
        sessions
      (roster--active-sessions sessions))))

(defun roster--entry (session)
  "Build one tabulated list entry for SESSION."
  (let ((directory (plist-get session :directory)))
    (list (roster--session-key session)
          (vector
           (propertize (roster--session-title session)
                       'face 'roster-title-face)
           (propertize (roster--tool-label session) 'face (roster--tool-face session))
           (propertize (upcase (roster--session-state session)) 'face (roster--state-face session))
           (propertize (file-name-nondirectory (directory-file-name directory))
                       'face 'roster-project-face)
           (propertize directory 'face 'roster-directory-face)
           (propertize (roster--format-time-millis (plist-get session :time-updated))
                       'face 'roster-time-face)))))

(defun roster--populate ()
  "Refresh `tabulated-list-entries' for the current `roster' buffer."
  (setq roster--sessions (roster--visible-sessions)
        tabulated-list-entries (mapcar #'roster--entry roster--sessions)))

(defun roster--session-at-point ()
  "Return the session at point in a `roster-mode' buffer."
  (let ((session-key (tabulated-list-get-id)))
    (unless session-key
      (user-error "No session on this line"))
    (or (roster--session-by-key session-key)
        (user-error "Session %s no longer exists" (cdr session-key)))))

(defun roster-refresh ()
  "Refresh the current `roster' list buffer."
  (interactive)
  (revert-buffer)
  (roster--apply-marks))

(defun roster-toggle-archived ()
  "Toggle whether archived sessions are shown in the current list."
  (interactive)
  (setq roster-show-archived (not roster-show-archived))
  (tabulated-list-revert)
  (message "%s archived sessions"
           (if roster-show-archived "Showing" "Hiding")))

(defun roster-resume (&optional arg)
  "Resume the session on the current line.
With a prefix ARG, open the session directory in Dired first."
  (interactive "P")
  (roster--resume-session (roster--session-at-point) arg))

(defun roster-open-directory ()
  "Open the current session's directory in Dired."
  (interactive)
  (dired (plist-get (roster--session-at-point) :directory)))

(defun roster-rename ()
  "Rename the session on the current line."
  (interactive)
  (when (roster--rename-session-command (roster--session-at-point))
    (tabulated-list-revert)))

(defun roster--toggle-archive-at-point ()
  "Toggle archived state for the session on the current line."
  (let* ((session (roster--session-at-point))
         (archived (not (roster--session-archived-p session))))
    (when (roster--set-archived-command session archived)
      (tabulated-list-revert))))

(defun roster-move-directory ()
  "Move the session on the current line to another project directory."
  (interactive)
  (when (roster--opencode-update-session-directory-command (roster--session-at-point))
    (tabulated-list-revert)))

(defun roster--delete-at-point ()
  "Delete the session on the current line."
  (let ((line (line-number-at-pos)))
    (when (roster--delete-session-command (roster--session-at-point))
      (tabulated-list-revert)
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (when (eobp)
        (forward-line -1)))))

(defun roster-new-session ()
  "Start a new session from the list buffer."
  (interactive)
  (roster--start-new-session-with-directory-prompt))

;;; Mark and bulk operations

(defun roster--marked-keys ()
  "Return backend-qualified keys of marked sessions in the current buffer."
  (let (keys)
    (maphash (lambda (key _) (push key keys)) roster--marked)
    (nreverse keys)))

(defun roster--delete-mark-overlays (ovs)
  "Delete overlay pair OVS (a cons of two overlays)."
  (when (overlayp (car ovs)) (delete-overlay (car ovs)))
  (when (overlayp (cdr ovs)) (delete-overlay (cdr ovs))))

(defun roster--clear-marks ()
  "Remove every mark and its overlay in the current buffer."
  (maphash (lambda (_id ovs) (roster--delete-mark-overlays ovs))
           roster--mark-overlays)
  (clrhash roster--marked)
  (clrhash roster--mark-overlays))

(defun roster--add-mark-overlay (session-id)
  "Highlight the current line as marked for SESSION-ID."
  (when-let ((existing (gethash session-id roster--mark-overlays)))
    (roster--delete-mark-overlays existing))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position)))
        (mark-ov (make-overlay (line-beginning-position)
                               (1+ (line-beginning-position)))))
    (overlay-put ov 'face 'roster-mark-face)
    (overlay-put mark-ov 'display (propertize "*" 'face 'roster-mark-indicator-face))
    (puthash session-id (cons ov mark-ov) roster--mark-overlays)))

(defun roster--apply-marks ()
  "Reapply mark overlays after a buffer refresh.
After `revert-buffer' the buffer is repopulated and all overlay positions
are stale.  We drop every existing overlay, then walk the new buffer to
rebuild them at their current line positions."
  (maphash (lambda (_id ovs) (roster--delete-mark-overlays ovs))
           roster--mark-overlays)
  (clrhash roster--mark-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((id (tabulated-list-get-id)))
        (when (and id (gethash id roster--marked))
          (roster--add-mark-overlay id)))
      (forward-line 1))))

(defun roster--nearest-surviving-session (deleted-keys)
  "Return the key of the session nearest to point not in DELETED-KEYS.
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
          (when (and id (not (member id deleted-keys)))
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

(defun roster--line-of-session (session-key)
  "Return the line number of SESSION-KEY in the current buffer, or nil."
  (when session-key
    (save-excursion
      (goto-char (point-min))
      (let (line)
        (while (and (not line) (< (point) (point-max)))
          (when (equal (tabulated-list-get-id) session-key)
            (setq line (line-number-at-pos)))
          (forward-line 1))
        line))))

(defun roster-mark ()
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
              (puthash id t roster--marked)
              (roster--add-mark-overlay id))
            (forward-line 1)))
        (deactivate-mark)
        (goto-char (max beg end))
        (beginning-of-line)
        (forward-line 1))
    (let ((id (tabulated-list-get-id)))
      (unless id (user-error "No session on this line"))
      (if (gethash id roster--marked)
          (progn
            (remhash id roster--marked)
            (when-let ((ovs (gethash id roster--mark-overlays)))
              (roster--delete-mark-overlays ovs)
              (remhash id roster--mark-overlays)))
        (puthash id t roster--marked)
        (roster--add-mark-overlay id))
      (forward-line 1))))

(defun roster-unmark ()
  "Unmark the session at point and advance to the next line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No session on this line"))
    (remhash id roster--marked)
    (when-let ((ovs (gethash id roster--mark-overlays)))
      (roster--delete-mark-overlays ovs)
      (remhash id roster--mark-overlays))
    (forward-line 1)))

(defun roster-unmark-all ()
  "Clear every mark in the current roster list buffer."
  (interactive)
  (roster--clear-marks)
  (message "Cleared all marks"))

(defun roster-delete ()
  "Delete all marked sessions after confirmation.
If no sessions are marked, delete the session on the current line."
  (interactive)
  (let ((keys (roster--marked-keys)))
    (if (null keys)
        (roster--delete-at-point)
      (when (yes-or-no-p (format "Delete %d marked sessions? " (length keys)))
        (let* (;; Capture the visual row offset of point within the window so
               ;; we can `recenter' to the same screen position after refresh.
               (win-line (count-screen-lines (window-start) (point)))
               (target-id (roster--nearest-surviving-session keys)))
          (dolist (key keys)
            (when-let ((session (roster--session-by-key key)))
              (roster--do-delete-session session)))
          (roster--clear-marks)
          (revert-buffer)
          (roster--apply-marks)
          (when-let ((ln (roster--line-of-session target-id)))
            (goto-char (point-min))
            (forward-line (1- ln))
            (recenter win-line))
          (message "Deleted %d sessions" (length keys)))))))

(defun roster-archive ()
  "Toggle archive state of all marked sessions after confirmation.
If no sessions are marked, toggle the session on the current line."
  (interactive)
  (let ((keys (roster--marked-keys)))
    (if (null keys)
        (roster--toggle-archive-at-point)
      (let* ((sessions (seq-keep #'roster--session-by-key keys))
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
                 (target-id (roster--nearest-surviving-session keys)))
            (dolist (session sessions)
              (roster--do-archive-session session
                                          (not (roster--session-archived-p session))))
            (roster--clear-marks)
            (revert-buffer)
            (roster--apply-marks)
            (when-let ((ln (roster--line-of-session target-id)))
              (goto-char (point-min))
              (forward-line (1- ln))
              (recenter win-line))
            (message "%s %d sessions" past (length sessions))))))))

;;; Mode definition

(defvar roster-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'roster-resume)
    (define-key map (kbd "e") #'roster-resume)
    (define-key map (kbd "d") #'roster-delete)
    (define-key map (kbd "r") #'roster-rename)
    (define-key map (kbd "a") #'roster-archive)
    (define-key map (kbd "R") #'roster-move-directory)
    (define-key map (kbd "o") #'roster-open-directory)
    (define-key map (kbd "c") #'roster-new-session)
    (define-key map (kbd "g") #'roster-refresh)
    (define-key map (kbd "t") #'roster-toggle-archived)
    (define-key map (kbd "m") #'roster-mark)
    (define-key map (kbd "u") #'roster-unmark)
    (define-key map (kbd "U") #'roster-unmark-all)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `roster-mode'.")

(define-derived-mode roster-mode tabulated-list-mode "roster"
  "Major mode for managing AI coding sessions."
  (setq-local roster--marked (make-hash-table :test #'equal))
  (setq-local roster--mark-overlays (make-hash-table :test #'equal))
  (setq-local roster--sessions nil)
  (setq tabulated-list-format [("Title"     28 t)
                               ("Tool"       4 t)
                               ("State"     10 t)
                               ("Project"   18 t)
                               ("Directory" 38 t)
                               ("Updated"   16 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'roster--populate nil t)
  (tabulated-list-init-header))

(defun roster--open-buffer (buffer-name source-function &optional include-archived)
  "Open a `roster' list BUFFER-NAME using SOURCE-FUNCTION.
When INCLUDE-ARCHIVED is non-nil, archived sessions are shown initially.
When omitted or nil, the value of `roster-include-archived' is used."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (roster-mode)
      (setq-local roster-source-function source-function)
      (setq-local roster-show-archived (if (null include-archived)
                                           roster-include-archived
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
  "Rename SESSION and return non-nil when its title is changed."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-rename-session-command session))
    ('codex  (roster--codex-rename-session-command session))
    ('pi     (roster--pi-rename-session-command session))
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

(defun roster--set-archived-command (session archived)
  "Set SESSION archived state to ARCHIVED and return non-nil on change."
  (let* ((session-id (roster--session-id session))
         (title (roster--session-title session))
         (verb (if archived "Archive" "Unarchive"))
         (past (if archived "Archived" "Unarchived")))
    (when (yes-or-no-p (format "%s session '%s' (%s)? " verb title session-id))
      (roster--do-archive-session session archived)
      (message "%s session %s" past session-id)
      t)))

(defun roster--do-delete-session (session)
  "Delete SESSION without prompting."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-delete-session session))
    ('codex  (roster--codex-delete-session session))
    ('pi     (roster--pi-delete-session session))
    (_       (roster--opencode-delete-session session))))

(defun roster--do-archive-session (session archived)
  "Archive SESSION when ARCHIVED is non-nil, otherwise unarchive it."
  (pcase (plist-get session :tool)
    ('claude (roster--claude-do-archive session archived))
    ('codex  (roster--codex-do-archive session archived))
    ('pi     (roster--pi-do-archive session archived))
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
Signals a `user-error' for Claude Code, Codex, and pi sessions, since
those backends do not support directory moves."
  (when (memq (plist-get session :tool) '(claude codex pi))
    (user-error "Directory moves are not supported for %s sessions"
                (pcase (plist-get session :tool)
                  ('claude "Claude Code")
                  ('codex  "Codex")
                  ('pi     "pi"))))
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
(defun roster ()
  "Open a Dired-like buffer for managing sessions."
  (interactive)
  (roster--open-buffer roster-buffer-name #'roster--load-sessions))

;;;###autoload
(defun roster-project ()
  "Open a Dired-like buffer for sessions in the current project scope."
  (interactive)
  (let ((scope (roster--project-scope-directory)))
    (roster--open-buffer
     (format "%s<%s>" roster-buffer-name (file-name-nondirectory (directory-file-name scope)))
     (lambda ()
       (roster--project-scoped-sessions (roster--load-sessions))))))

(provide 'roster)

;;; roster.el ends here
