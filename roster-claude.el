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

;; Session listing, renaming and deletion go through Claude Agent SDK's
;; session API, called via a small Node bridge.  The SDK lives in a local
;; npm prefix installed by `roster-claude-install-sdk'.  Archive has no
;; native equivalent, so roster keeps it in a sidecar file per session.
;; Renaming also writes the `agent-name' entry the CLI's `/rename' writes,
;; and is refused while a Claude Code process has the session open.

(defconst roster--claude-package-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the roster package.")

(defconst roster--claude-sdk-script
  (expand-file-name "scripts/roster-claude-sdk.mjs" roster--claude-package-dir)
  "Path to roster's Claude Agent SDK bridge.")

(defconst roster--claude-sdk-package "@anthropic-ai/claude-agent-sdk@0.3.302"
  "Pinned npm package spec of the Claude Agent SDK used by roster.")

(defcustom roster-claude-node-command "node"
  "Node.js executable used to run the Claude Agent SDK bridge."
  :type 'string
  :group 'roster)

(defcustom roster-claude-npm-command "npm"
  "The npm executable used by `roster-claude-install-sdk'."
  :type 'string
  :group 'roster)

(defcustom roster-claude-sdk-directory
  (expand-file-name "claude-sdk" roster--claude-package-dir)
  "Local npm prefix holding roster's Claude Agent SDK installation.
`roster-claude-install-sdk' installs the SDK into its `node_modules'."
  :type 'directory
  :group 'roster)

(defun roster-claude-install-sdk ()
  "Install the pinned Claude Agent SDK into `roster-claude-sdk-directory'.
Optional dependencies, which only carry the bundled Claude Code binary,
are omitted: roster uses the SDK's session API, not its agent runtime."
  (interactive)
  (make-directory roster-claude-sdk-directory t)
  (let ((default-directory (file-name-as-directory roster-claude-sdk-directory)))
    (compilation-start
     (mapconcat #'shell-quote-argument
                (list roster-claude-npm-command "install"
                      "--prefix" roster-claude-sdk-directory
                      "--omit=optional" "--ignore-scripts"
                      "--no-audit" "--no-fund"
                      roster--claude-sdk-package)
                " ")
     nil (lambda (_mode) "*roster-claude-install*"))))

(defun roster--claude-sdk-call (&rest arguments)
  "Call the Claude Agent SDK bridge with ARGUMENTS and return its result.
Signal a `user-error' when the bridge cannot run, the SDK is not
installed, or the SDK reports an error."
  (with-temp-buffer
    (let* ((process-environment
            (append (list (concat "CLAUDE_CONFIG_DIR=" roster-claude-dir)
                          (concat "ROSTER_CLAUDE_SDK_DIR="
                                  (expand-file-name roster-claude-sdk-directory)))
                    process-environment))
           (status (condition-case err
                       (apply #'call-process roster-claude-node-command nil t nil
                              roster--claude-sdk-script arguments)
                     (file-missing
                      (user-error "Cannot run Claude Agent SDK bridge: %s"
                                  (error-message-string err)))))
           (result (roster--read-json (string-trim (buffer-string)))))
      (cond
       ((and (plist-member result :available)
             (not (plist-get result :available)))
        (user-error "Claude Agent SDK is not installed; run %s"
                    (substitute-command-keys
                     "\\[roster-claude-install-sdk]")))
       ((not (eq status 0))
        (user-error "Claude Agent SDK error: %s"
                    (or (plist-get result :error)
                        (string-trim (buffer-string)))))
       (t result)))))

(defun roster--claude-roster-dir ()
  "Return the directory used to store Claude Code roster sidecar files."
  (expand-file-name "roster" roster-claude-dir))

(defun roster--claude-sidecar-path (session-id)
  "Return the path to the roster sidecar JSON file for SESSION-ID."
  (expand-file-name (concat session-id ".roster.json")
                    (roster--claude-roster-dir)))

(defun roster--claude-time-archived (session-id)
  "Return roster's archive time for Claude SESSION-ID, or nil."
  (let ((value (cdr (assoc "time_archived"
                           (roster--read-sidecar
                            (roster--claude-sidecar-path session-id))))))
    (when (numberp value) value)))

(defun roster--claude-session-from-sdk (value)
  "Return unified Claude session parsed from SDK bridge VALUE."
  (let* ((session-id (plist-get value :id))
         (title (plist-get value :title))
         (directory (plist-get value :directory)))
    (list :id session-id
          :title (if (and (stringp title) (not (string-empty-p title)))
                     (roster--truncate-title title)
                   roster--untitled)
          :directory (expand-file-name
                      (if (and (stringp directory) (not (string-empty-p directory)))
                          directory
                        "~"))
          :project-id nil
          :time-updated (or (plist-get value :time_updated) 0)
          :time-archived (roster--claude-time-archived session-id)
          :tool 'claude)))

(defun roster--claude-load-sessions (&optional include-archived)
  "Return Claude Code sessions, optionally INCLUDE-ARCHIVED sessions."
  (let ((sessions (mapcar #'roster--claude-session-from-sdk
                          (plist-get (roster--claude-sdk-call "list")
                                     :sessions))))
    (if include-archived sessions (roster--active-sessions sessions))))

(defun roster--claude-delete-session (session)
  "Delete Claude Code SESSION through Agent SDK, with its roster sidecar.
The SDK removes the transcript and its subagent directory permanently."
  (let* ((session-id (roster--session-id session))
         (sidecar (roster--claude-sidecar-path session-id)))
    (roster--claude-sdk-call "delete" session-id)
    (when (file-exists-p sidecar)
      (delete-file sidecar))))

(defun roster--claude-rename-session (session new-title)
  "Rename Claude Code SESSION to NEW-TITLE through Agent SDK."
  (roster--claude-sdk-call "rename" (roster--session-id session) new-title)
  t)

(defun roster--claude-do-archive (session archived)
  "Set a Claude Code SESSION archived state to ARCHIVED without prompting."
  ;; Local Claude CLI sessions have no archive API.  Archive is therefore a
  ;; roster-only view preference, kept in a sidecar rather than corrupting the
  ;; SDK's single user-controlled tag field.
  (roster--write-sidecar
   (roster--claude-sidecar-path (roster--session-id session))
   nil
   (when archived
     (floor (* roster--ms-per-second (float-time (current-time)))))))

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
