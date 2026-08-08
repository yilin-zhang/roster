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
;; Each backend module owns its storage, commands, mutations, and display metadata.
;; `roster.el' consumes those capabilities through the registry in `roster-core'.
;;
;; Requires Emacs 29.1+ for built-in SQLite support (sqlite.el).

;;; Code:

(require 'roster-core)
(require 'roster-opencode)
(require 'roster-claude)
(require 'roster-codex)
(require 'roster-pi)

;;; Tool helpers

(defun roster--load-tool-sessions (tool include-archived)
  "Load TOOL sessions, optionally including archived sessions."
  (roster--backend-call (roster--backend tool)
                        #'roster-backend-load "session loading"
                        include-archived))

(defun roster--load-sessions (&optional include-archived)
  "Return enabled-tool sessions as a unified list, newest-first.
Load archived sessions when INCLUDE-ARCHIVED or `roster-show-archived' is non-nil."
  (roster--sort-sessions
   (seq-mapcat
    (lambda (tool)
      (condition-case err
          (roster--load-tool-sessions
           tool (or include-archived roster-show-archived))
        (error (message "roster: %s sessions unavailable: %s"
                        tool (error-message-string err))
               nil)))
    (roster--enabled-tools))))

(defun roster--tool-label (session)
  "Return the short tool tag string for SESSION."
  (roster-backend-label (roster--session-backend session)))

(defun roster--tool-face (session)
  "Return the face for SESSION's tool tag."
  (roster-backend-face (roster--session-backend session)))

(defun roster--session-command (session)
  "Return the shell command used to resume SESSION."
  (roster--session-backend-call
   session #'roster-backend-resume-command "resume"))

(defun roster--new-session-command (tool)
  "Return the command used to create a new TOOL session."
  (roster--backend-call (roster--backend tool)
                        #'roster-backend-new-command "new sessions"))

(defun roster--select-tool-for-new-session ()
  "Return the tool symbol to use for a new session."
  (let ((tools
         (seq-filter
          (lambda (tool)
            (when-let ((backend (gethash tool roster--backends)))
              (roster-backend-new-command backend)))
          (roster--enabled-tools))))
    (cond
     ((null tools) (user-error "No enabled roster backends are registered"))
     ((cdr tools)
      (intern (completing-read
               "Tool: "
               (mapcar #'symbol-name tools)
               nil t nil nil
               (when (memq roster-default-new-session-tool tools)
                 (symbol-name roster-default-new-session-tool)))))
     (t (car tools)))))

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

(defun roster--resume-session (session &optional jump terminal-function)
  "Resume SESSION in a terminal window.
When JUMP is non-nil, open the session directory in Dired first.
TERMINAL-FUNCTION overrides `roster-terminal-function' when non-nil."
  (let ((directory (roster--session-directory session)))
    (when jump
      (dired directory))
    (funcall (or terminal-function roster-terminal-function)
             directory (roster--session-command session))))

;;; Mode internals

(defun roster--session-by-key (session-key)
  "Return the cached session for backend-qualified SESSION-KEY."
  (when session-key
    (seq-find (lambda (session)
                (equal (roster--session-key session) session-key))
              roster--sessions)))

(defun roster--visible-sessions ()
  "Return sessions for the current `roster' list buffer."
  (if roster-show-archived
      roster--all-sessions
    (roster--active-sessions roster--all-sessions)))

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

(defun roster--render-sessions ()
  "Build visible session state from the current loaded snapshot."
  (setq roster--sessions (roster--visible-sessions)
        tabulated-list-entries (mapcar #'roster--entry roster--sessions)))

(defun roster--populate ()
  "Reload the current roster source and rebuild its visible entries."
  (unless roster-source-function
    (user-error "No session source configured for this roster buffer"))
  (let ((include-archived
         (or (not roster-backend-source-p)
             roster-show-archived
             roster--snapshot-includes-archived)))
    (setq roster--all-sessions
          (funcall roster-session-filter-function
                   (if roster-backend-source-p
                       (funcall roster-source-function include-archived)
                     (funcall roster-source-function)))
          roster--snapshot-includes-archived include-archived))
  (roster--render-sessions))

(defun roster--redisplay-sessions ()
  "Render the loaded snapshot without consulting any backend."
  (roster--render-sessions)
  (tabulated-list-print t)
  (roster--apply-marks))

(defun roster--reload-tools (tools)
  "Reload TOOLS in the current snapshot, preserving other backend results."
  (if (not roster-backend-source-p)
      (progn
        (tabulated-list-revert)
        (roster--apply-marks))
    (dolist (tool (seq-uniq tools #'eq))
      (condition-case err
          (let ((sessions
                 (roster--load-tool-sessions
                  tool roster--snapshot-includes-archived)))
            (setq roster--all-sessions
                  (append
                   (seq-remove (lambda (session)
                                 (eq (roster--session-tool session) tool))
                               roster--all-sessions)
                   (funcall roster-session-filter-function sessions))))
        (error
         (message "roster: %s refresh failed; showing local result: %s"
                  tool (error-message-string err)))))
    (setq roster--all-sessions (roster--sort-sessions roster--all-sessions))
    (roster--redisplay-sessions)))

(defun roster--session-at-point ()
  "Return the session at point in a `roster-mode' buffer."
  (let ((session-key (tabulated-list-get-id)))
    (unless session-key
      (user-error "No session on this line"))
    (or (roster--session-by-key session-key)
        (user-error "Session %s no longer exists" (cdr session-key)))))

(defun roster-refresh (&optional hard)
  "Refresh the current roster list buffer.
With prefix argument HARD, clear backend caches before reloading."
  (interactive "P")
  (when hard
    (run-hooks 'roster-clear-caches-hook))
  (revert-buffer)
  (roster--apply-marks))

(defun roster-toggle-archived ()
  "Toggle whether archived sessions are shown in the current list."
  (interactive)
  (setq roster-show-archived (not roster-show-archived))
  (if (and roster-show-archived
           roster-backend-source-p
           (not roster--snapshot-includes-archived))
      (progn
        (tabulated-list-revert)
        (roster--apply-marks))
    (roster--redisplay-sessions))
  (message "%s archived sessions"
           (if roster-show-archived "Showing" "Hiding")))

(defun roster-resume (&optional arg)
  "Resume the session on the current line.
With a prefix ARG, open the session directory in Dired first."
  (interactive "P")
  (roster--resume-session (roster--session-at-point) arg))

(defun roster-resume-with-terminal ()
  "Choose an available terminal and resume the session on the current line."
  (interactive)
  (roster--resume-session (roster--session-at-point) nil
                          (roster--read-terminal-function)))

(defun roster-open-directory ()
  "Open the current session's directory in Dired."
  (interactive)
  (dired (plist-get (roster--session-at-point) :directory)))

(defun roster-rename ()
  "Rename the session on the current line."
  (interactive)
  (let ((session (roster--session-at-point)))
    (when (roster--rename-session-command session)
      (roster--reload-tools (list (roster--session-tool session))))))

(defun roster--toggle-archive-at-point ()
  "Toggle archived state for the session on the current line."
  (let* ((session (roster--session-at-point))
         (archived (not (roster--session-archived-p session))))
    (when (roster--set-archived-command session archived)
      (roster--reload-tools (list (roster--session-tool session))))))

(defun roster-move-directory ()
  "Move the session on the current line to another project directory."
  (interactive)
  (let ((session (roster--session-at-point)))
    (when (roster--move-session-command session)
      (roster--reload-tools (list (roster--session-tool session))))))

(defun roster--delete-at-point ()
  "Delete the session on the current line."
  (let ((line (line-number-at-pos))
        (session (roster--session-at-point)))
    (when (roster--delete-session-command session)
      (setq roster--all-sessions
            (delq session roster--all-sessions))
      (roster--reload-tools (list (roster--session-tool session)))
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

(defun roster--for-each-session-by-backend (function sessions)
  "Call FUNCTION for SESSIONS, sharing each backend's batch scope."
  (dolist (group (seq-group-by #'roster--session-tool sessions))
    (let ((backend (roster--backend (car group)))
          (backend-sessions (cdr group)))
      (roster--call-with-backend-batch
       backend
       (lambda () (mapc function backend-sessions))))))

(defun roster-delete ()
  "Delete all marked sessions after confirmation.
If no sessions are marked, delete the session on the current line."
  (interactive)
  (let ((keys (roster--marked-keys)))
    (if (null keys)
        (roster--delete-at-point)
      (when (yes-or-no-p (format "Delete %d marked sessions? " (length keys)))
        (let* ((sessions (seq-keep #'roster--session-by-key keys))
               (tools (mapcar #'roster--session-tool sessions))
               ;; Capture the visual row offset of point within the window so
               ;; we can `recenter' to the same screen position after refresh.
               (win-line (count-screen-lines (window-start) (point)))
               (target-id (roster--nearest-surviving-session keys)))
          (roster--for-each-session-by-backend
           #'roster--do-delete-session sessions)
          (setq roster--all-sessions
                (seq-remove (lambda (session)
                              (member (roster--session-key session) keys))
                            roster--all-sessions))
          (roster--clear-marks)
          (roster--reload-tools tools)
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
             (tools (mapcar #'roster--session-tool sessions))
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
            (roster--for-each-session-by-backend
             (lambda (session)
               (let ((archived (not (roster--session-archived-p session))))
                 (roster--do-archive-session session archived)
                 (plist-put session :time-archived
                            (and archived
                                 (floor (* roster--ms-per-second
                                           (float-time)))))))
             sessions)
            (roster--clear-marks)
            (roster--reload-tools tools)
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
    (define-key map (kbd "S-<return>") #'roster-resume-with-terminal)
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
  (setq-local roster--all-sessions nil)
  (setq-local roster--snapshot-includes-archived nil)
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

(defun roster--open-buffer (buffer-name source-function &optional include-archived
                                        filter-function backend-source-p)
  "Open a `roster' list BUFFER-NAME using SOURCE-FUNCTION.
When INCLUDE-ARCHIVED is non-nil, archived sessions are shown initially.
When omitted or nil, the value of `roster-include-archived' is used.
FILTER-FUNCTION optionally limits the loaded snapshot for a specialized view.
BACKEND-SOURCE-P means SOURCE-FUNCTION accepts an include-archived argument
and supports tool-specific reloads."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (roster-mode)
      (setq-local roster-source-function source-function)
      (setq-local roster-session-filter-function
                  (or filter-function #'identity))
      (setq-local roster-backend-source-p backend-source-p)
      (setq-local roster-show-archived (if (null include-archived)
                                           roster-include-archived
                                         include-archived))
      (tabulated-list-revert))
    (pop-to-buffer buffer)))

;;; Command dispatch

(defun roster--rename-session-command (session)
  "Rename SESSION and return non-nil when its title is changed."
  (let* ((session-id (roster--session-id session))
         (old-title (roster--session-title session))
         (new-title (roster--read-session-title session)))
    (if (string= old-title new-title)
        (progn
          (message "Session %s already uses that title" session-id)
          nil)
      (roster--session-backend-call
       session #'roster-backend-rename "rename" new-title)
      (plist-put session :title new-title)
      (message "Renamed session %s to %s" session-id new-title)
      t)))

(defun roster--set-archived-command (session archived)
  "Set SESSION archived state to ARCHIVED and return non-nil on change."
  (let* ((session-id (roster--session-id session))
         (title (roster--session-title session))
         (verb (if archived "Archive" "Unarchive"))
         (past (if archived "Archived" "Unarchived")))
    (when (yes-or-no-p (format "%s session '%s' (%s)? " verb title session-id))
      (roster--do-archive-session session archived)
      (plist-put session :time-archived
                 (and archived
                      (floor (* roster--ms-per-second (float-time)))))
      (message "%s session %s" past session-id)
      t)))

(defun roster--do-delete-session (session)
  "Delete SESSION without prompting."
  (roster--session-backend-call
   session #'roster-backend-delete "deletion"))

(defun roster--do-archive-session (session archived)
  "Archive SESSION when ARCHIVED is non-nil, otherwise unarchive it."
  (roster--session-backend-call
   session #'roster-backend-archive "archiving" archived))

(defun roster--delete-session-command (session)
  "Delete SESSION and return non-nil on success."
  (let ((session-id (roster--session-id session))
        (title (roster--session-title session))
        (directory (roster--session-directory session)))
    (when (yes-or-no-p (format "Delete session '%s' (%s)? " title session-id))
      (roster--do-delete-session session)
      (message "Deleted session %s from %s" session-id directory)
      t)))

(defun roster--move-session-command (session)
  "Move SESSION using its backend capability."
  (let ((directory
         (roster--session-backend-call
          session #'roster-backend-move "directory moves")))
    (when (stringp directory)
      (plist-put session :directory directory))
    directory))

;;; Public commands

;;;###autoload
(defun roster ()
  "Open a Dired-like buffer for managing sessions."
  (interactive)
  (roster--open-buffer roster-buffer-name
                       #'roster--load-sessions nil nil t))

;;;###autoload
(defun roster-project ()
  "Open a Dired-like buffer for sessions in the current project scope."
  (interactive)
  (let ((scope (roster--project-scope-directory)))
    (roster--open-buffer
     (format "%s<%s>" roster-buffer-name (file-name-nondirectory (directory-file-name scope)))
     #'roster--load-sessions
     nil
     #'roster--project-scoped-sessions
     t)))

(provide 'roster)

;;; roster.el ends here
