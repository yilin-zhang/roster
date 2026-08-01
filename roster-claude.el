;;; roster-claude.el --- Claude Code backend for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-core)

;;; Claude Code backend

(defface roster-tool-claude-face
  `((t :foreground ,(face-attribute 'ansi-color-yellow :foreground)))
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
        (let ((text (roster--content-text
                     (plist-get (plist-get obj :message) :content))))
          ;; Skip system-injected turns such as /compact output, which Claude
          ;; Code stores as user messages beginning with an XML-like tag (e.g.
          ;; "<local-command-caveat>...").  Those are not real session starters.
          (unless (and text (string-prefix-p "<" text))
            (setq new-title text)))))
    (list :slug new-slug
          :cwd new-cwd
          :title-candidate new-title)))

(defun roster--claude-recorded-titles (path)
  "Return the latest explicit and generated titles recorded at PATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-max))
        (let (custom-title ai-title)
          (while (and (not (bobp))
                      (not (and custom-title ai-title)))
            (forward-line -1)
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (obj (roster--read-json line)))
              (pcase (plist-get obj :type)
                ("custom-title"
                 (unless custom-title
                   (let ((value (plist-get obj :customTitle)))
                     (when (and (stringp value) (not (string-empty-p value)))
                       (setq custom-title value)))))
                ("ai-title"
                 (unless ai-title
                   (let ((value (plist-get obj :aiTitle)))
                     (when (and (stringp value) (not (string-empty-p value)))
                       (setq ai-title value))))))))
          (list :custom-title custom-title :ai-title ai-title)))
    (error nil)))

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
:ai-title, and :time-updated (file mtime in milliseconds)."
  (condition-case nil
      (let (slug cwd title-candidate)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (and (not (eobp))
                      (not (and slug cwd title-candidate)))
            (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
                   (obj (roster--read-json line)))
              (when obj
                (pcase-let ((`(:slug ,new-slug :cwd ,new-cwd :title-candidate ,new-title)
                             (roster--claude-update-meta-from-object obj slug cwd title-candidate)))
                  (setq slug new-slug
                        cwd new-cwd
                        title-candidate new-title))))
            (forward-line 1)))
        (let* ((recorded-titles (roster--claude-recorded-titles path))
               (attrs (file-attributes path))
               (mtime (when attrs (file-attribute-modification-time attrs)))
               (time-updated (if mtime
                                 (floor (* roster--ms-per-second (float-time mtime)))
                               0)))
          (list :slug slug
                :cwd (or cwd "")
                :title-candidate title-candidate
                :custom-title (plist-get recorded-titles :custom-title)
                :ai-title (plist-get recorded-titles :ai-title)
                :time-updated time-updated)))
    (error nil)))

(defun roster--claude-load-sessions (&optional include-archived)
  "Return Claude Code sessions, optionally INCLUDE-ARCHIVED sessions."
  (let ((projects-dir (roster--claude-projects-dir)))
    (when (file-directory-p projects-dir)
      (let ((sessions
             (delq nil
                   (mapcar (lambda (entry)
                             (roster--claude-session-from-file
                              (car entry) (cdr entry)))
                           (roster--claude-jsonl-files projects-dir)))))
        (if include-archived sessions (roster--active-sessions sessions))))))

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

(defun roster--claude-rename-session (session new-title)
  "Rename Claude Code SESSION to NEW-TITLE."
  (roster--claude-append-custom-title
   (plist-get session :encoded-dir) (roster--session-id session) new-title))

(defun roster--claude-do-archive (session archived)
  "Set a Claude Code SESSION archived state to ARCHIVED without prompting."
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
