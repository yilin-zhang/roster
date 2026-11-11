;;; roster-claude-test.el --- Claude Code tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Claude Agent SDK backend.

;;; Code:

(require 'roster-test-helpers)

(defmacro roster-claude-test-with-root (&rest body)
  "Run BODY with `roster-claude-dir' bound to a fresh temporary directory."
  (declare (indent 0))
  `(let* ((root (make-temp-file "roster-claude-root-" t))
          (roster-claude-dir root))
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

(defun roster-claude-test-sdk-result (&rest sessions)
  "Return a bridge `list' result containing SESSIONS."
  (list :available t :sessions sessions))

;;; Session loading

(ert-deftest roster-claude-load-sessions-maps-sdk-sessions ()
  (roster-claude-test-with-root
    (cl-letf (((symbol-function 'roster--claude-sdk-call)
               (lambda (&rest _)
                 (roster-claude-test-sdk-result
                  (list :id "cc-1" :title "SDK title"
                        :directory "/tmp/sdk" :time_updated 42)))))
      (let ((session (car (roster--claude-load-sessions))))
        (should (equal (plist-get session :id) "cc-1"))
        (should (equal (plist-get session :title) "SDK title"))
        (should (equal (plist-get session :directory) "/tmp/sdk"))
        (should (= (plist-get session :time-updated) 42))
        (should-not (plist-get session :time-archived))
        (should (eq (plist-get session :tool) 'claude))))))

(ert-deftest roster-claude-load-sessions-handles-missing-fields ()
  "Long titles are truncated; missing titles and directories get defaults."
  (roster-claude-test-with-root
    (cl-letf (((symbol-function 'roster--claude-sdk-call)
               (lambda (&rest _)
                 (roster-claude-test-sdk-result
                  (list :id "long" :title (make-string 100 ?x)
                        :directory "/tmp")
                  (list :id "bare" :title "" :directory nil)))))
      (pcase-let ((`(,long ,bare) (roster--claude-load-sessions)))
        (should (= (length (plist-get long :title)) roster--title-max-length))
        (should (equal (plist-get bare :title) roster--untitled))
        (should (equal (plist-get bare :directory) (expand-file-name "~")))
        (should (= (plist-get bare :time-updated) 0))))))

(ert-deftest roster-claude-load-sessions-applies-archive-sidecar ()
  (roster-claude-test-with-root
    (roster--claude-do-archive '(:id "cc-old" :tool claude) t)
    (cl-letf (((symbol-function 'roster--claude-sdk-call)
               (lambda (&rest _)
                 (roster-claude-test-sdk-result
                  (list :id "cc-old" :title "Old" :directory "/tmp")
                  (list :id "cc-new" :title "New" :directory "/tmp")))))
      (should (equal (mapcar #'roster--session-id
                             (roster--claude-load-sessions))
                     '("cc-new")))
      (let ((sessions (roster--claude-load-sessions t)))
        (should (equal (mapcar #'roster--session-id sessions)
                       '("cc-old" "cc-new")))
        (should (roster--session-archived-p (car sessions)))))))

;;; Mutations

(ert-deftest roster-claude-archive-round-trips-through-sidecar ()
  (roster-claude-test-with-root
    (let ((session '(:id "cc-1" :tool claude)))
      (roster--claude-do-archive session t)
      (should (numberp (roster--claude-time-archived "cc-1")))
      (roster--claude-do-archive session nil)
      (should-not (roster--claude-time-archived "cc-1")))))

(ert-deftest roster-claude-rename-uses-sdk ()
  (let (call)
    (cl-letf (((symbol-function 'roster--claude-sdk-call)
               (lambda (&rest arguments) (setq call arguments) '(:available t))))
      (should (roster--claude-rename-session
               '(:id "cc-1" :directory "/tmp/sdk" :tool claude) "New title"))
      (should (equal call '("rename" "cc-1" "New title"))))))

(ert-deftest roster-claude-delete-uses-sdk-and-removes-sidecar ()
  (roster-claude-test-with-root
    (let ((session '(:id "cc-1" :tool claude))
          call)
      (roster--claude-do-archive session t)
      (cl-letf (((symbol-function 'roster--claude-sdk-call)
                 (lambda (&rest arguments) (setq call arguments) '(:available t))))
        (roster--claude-delete-session session))
      (should (equal call '("delete" "cc-1")))
      (should-not (file-exists-p (roster--claude-sidecar-path "cc-1"))))))

(ert-deftest roster-claude-delete-keeps-sidecar-when-sdk-fails ()
  (roster-claude-test-with-root
    (let ((session '(:id "cc-1" :tool claude)))
      (roster--claude-do-archive session t)
      (cl-letf (((symbol-function 'roster--claude-sdk-call)
                 (lambda (&rest _) (user-error "Claude Agent SDK error: boom"))))
        (should-error (roster--claude-delete-session session)
                      :type 'user-error))
      (should (file-exists-p (roster--claude-sidecar-path "cc-1"))))))

;;; Bridge protocol

(ert-deftest roster-claude-sdk-call-requires-sdk ()
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) (insert "{\"available\":false}") 0)))
    (should-error (roster--claude-sdk-call "list") :type 'user-error)))

(ert-deftest roster-claude-sdk-call-requires-node ()
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) (signal 'file-missing '("node")))))
    (should-error (roster--claude-sdk-call "list") :type 'user-error)))

(ert-deftest roster-claude-sdk-call-reports-sdk-errors ()
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _)
               (insert "{\"available\":true,\"error\":\"Session x not found\"}")
               1)))
    (let ((err (should-error (roster--claude-sdk-call "delete" "x")
                             :type 'user-error)))
      (should (string-match-p "Session x not found"
                              (error-message-string err))))))

(ert-deftest roster-claude-sdk-call-passes-directories ()
  (let ((roster-claude-dir "/tmp/claude-config")
        (roster-claude-sdk-directory "/tmp/claude-sdk")
        environment)
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _)
                 (setq environment process-environment)
                 (insert "{\"available\":true,\"sessions\":[]}")
                 0)))
      (roster--claude-sdk-call "list")
      (should (member "CLAUDE_CONFIG_DIR=/tmp/claude-config" environment))
      (should (member "ROSTER_CLAUDE_SDK_DIR=/tmp/claude-sdk" environment)))))

(defconst roster-claude-test-fake-sdk
  "import { appendFileSync } from 'node:fs';

const log = (...call) =>
  appendFileSync(process.env.ROSTER_FAKE_SDK_LOG, JSON.stringify(call) + '\\n');

export async function listSessions() {
  return ['cc-cli', 'cc-sdk', 'cc-legacy', 'cc-missing'].map((id) => ({
    sessionId: id, summary: 'Title ' + id, cwd: '/tmp/proj', lastModified: 7,
  }));
}

export async function renameSession(id, title) { log('rename', id, title); }

export async function deleteSession(id) { log('delete', id); }
"
  "Stand-in Claude Agent SDK module used to exercise the Node bridge.")

(ert-deftest roster-claude-bridge-filters-non-cli-sessions ()
  "The real bridge hides SDK-started runs and keeps legacy transcripts.
Subagent transcripts nested under a session directory are never consulted."
  (skip-unless (executable-find roster-claude-node-command))
  (roster-claude-test-with-root
    (let* ((roster-claude-sdk-directory (expand-file-name "sdk" root))
           (package-dir (expand-file-name
                         "node_modules/@anthropic-ai/claude-agent-sdk"
                         roster-claude-sdk-directory))
           (project-dir (expand-file-name "projects/-tmp-proj" root))
           (subagent-dir (expand-file-name "cc-legacy/subagents" project-dir))
           (process-environment
            (cons (concat "ROSTER_FAKE_SDK_LOG="
                          (expand-file-name "calls.jsonl" root))
                  process-environment)))
      (make-directory package-dir t)
      (make-directory subagent-dir t)
      (with-temp-file (expand-file-name "package.json" package-dir)
        (insert "{\"name\":\"@anthropic-ai/claude-agent-sdk\","
                "\"type\":\"module\",\"exports\":{\".\":\"./sdk.mjs\"}}"))
      (with-temp-file (expand-file-name "sdk.mjs" package-dir)
        (insert roster-claude-test-fake-sdk))
      (with-temp-file (expand-file-name "cc-cli.jsonl" project-dir)
        (insert "{\"type\":\"permission-mode\"}\n"
                "{\"type\":\"user\",\"entrypoint\":\"cli\"}\n"))
      (with-temp-file (expand-file-name "cc-sdk.jsonl" project-dir)
        (insert "{\"type\":\"user\",\"entrypoint\":\"sdk-ts\"}\n"))
      (with-temp-file (expand-file-name "cc-legacy.jsonl" project-dir)
        (insert "{\"type\":\"user\"}\n"))
      (with-temp-file (expand-file-name "cc-legacy.jsonl" subagent-dir)
        (insert "{\"type\":\"user\",\"entrypoint\":\"sdk-ts\"}\n"))
      (should (equal (mapcar #'roster--session-id
                             (roster--claude-load-sessions t))
                     '("cc-cli" "cc-legacy" "cc-missing")))
      (roster--claude-rename-session '(:id "cc-cli") "Renamed")
      (roster--claude-delete-session '(:id "cc-cli"))
      (should (equal (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "calls.jsonl" root))
                       (mapcar #'roster--read-json
                               (split-string (buffer-string) "\n" t)))
                     '(("rename" "cc-cli" "Renamed")
                       ("delete" "cc-cli")))))))

(ert-deftest roster-claude-bridge-renames-like-cli ()
  "Renaming writes the CLI's `agent-name' entry after the SDK's title.
Renaming or deleting a session held by a running Claude Code process is
refused, while a registry entry whose pid now belongs to another process
is ignored."
  (skip-unless (executable-find roster-claude-node-command))
  (roster-claude-test-with-root
    (let* ((roster-claude-sdk-directory (expand-file-name "sdk" root))
           (package-dir (expand-file-name
                         "node_modules/@anthropic-ai/claude-agent-sdk"
                         roster-claude-sdk-directory))
           (project-dir (expand-file-name "projects/-tmp-proj" root))
           (sessions-dir (expand-file-name "sessions" root))
           (transcript (expand-file-name "cc-cli.jsonl" project-dir))
           (log (expand-file-name "calls.jsonl" root))
           (process-environment
            (cons (concat "ROSTER_FAKE_SDK_LOG=" log) process-environment)))
      (make-directory package-dir t)
      (make-directory project-dir t)
      (make-directory sessions-dir t)
      (with-temp-file (expand-file-name "package.json" package-dir)
        (insert "{\"name\":\"@anthropic-ai/claude-agent-sdk\","
                "\"type\":\"module\",\"exports\":{\".\":\"./sdk.mjs\"}}"))
      (with-temp-file (expand-file-name "sdk.mjs" package-dir)
        (insert roster-claude-test-fake-sdk))
      (with-temp-file transcript
        (insert "{\"type\":\"user\",\"entrypoint\":\"cli\"}\n"))
      ;; Emacs itself stands in for the running CLI.
      (with-temp-file (expand-file-name "1.json" sessions-dir)
        (insert (format "{\"pid\":%d,\"sessionId\":\"cc-cli\"}" (emacs-pid))))
      (let ((err (should-error (roster--claude-rename-session
                                '(:id "cc-cli") "Busy")
                               :type 'user-error)))
        (should (string-match-p "/rename" (error-message-string err))))
      (should-error (roster--claude-delete-session '(:id "cc-cli"))
                    :type 'user-error)
      (should-not (file-exists-p log))
      (with-temp-file (expand-file-name "1.json" sessions-dir)
        (insert (format "{\"pid\":%d,\"sessionId\":\"cc-cli\",%s}"
                        (emacs-pid)
                        "\"procStart\":\"Thu Jan  1 00:00:00 1970\"")))
      (roster--claude-rename-session '(:id "cc-cli") " Renamed ")
      (should (file-exists-p log))
      (should (equal (roster--read-json
                      (car (last (with-temp-buffer
                                   (insert-file-contents transcript)
                                   (split-string (buffer-string) "\n" t)))))
                     '(:type "agent-name" :agentName "Renamed"
                             :sessionId "cc-cli"))))))

(ert-deftest roster-claude-bridge-reports-missing-sdk ()
  (skip-unless (executable-find roster-claude-node-command))
  (roster-claude-test-with-root
    (let ((roster-claude-sdk-directory (expand-file-name "empty" root)))
      (let ((err (should-error (roster--claude-load-sessions)
                               :type 'user-error)))
        (should (string-match-p "roster-claude-install-sdk"
                                (error-message-string err)))))))

(ert-deftest roster-claude-install-sdk-uses-local-prefix ()
  (roster-claude-test-with-root
    (let ((roster-claude-sdk-directory (expand-file-name "sdk" root))
          command)
      (cl-letf (((symbol-function 'compilation-start)
                 (lambda (cmd &rest _) (setq command cmd))))
        (roster-claude-install-sdk))
      (should (file-directory-p roster-claude-sdk-directory))
      (should (string-match-p
               (regexp-quote (concat "--prefix "
                                     (shell-quote-argument
                                      roster-claude-sdk-directory)))
               command))
      (should (string-match-p (regexp-quote (shell-quote-argument "--omit=optional"))
                              command))
      (should (string-match-p (regexp-quote
                               (shell-quote-argument roster--claude-sdk-package))
                              command)))))

;;; Integration with the list

(ert-deftest roster-load-sessions-merges-opencode-and-claude ()
  (let ((roster-enabled-tools '(opencode claude)))
    (roster-claude-test-with-root
      (cl-letf (((symbol-function 'roster--opencode-load-sessions)
                 (lambda (&optional _)
                   (list (list :id "oc_1" :title "OpenCode Title"
                               :directory "/tmp/opencode" :project-id "proj_1"
                               :time-updated 1700000000000 :tool 'opencode))))
                ((symbol-function 'roster--claude-sdk-call)
                 (lambda (&rest _)
                   (roster-claude-test-sdk-result
                    (list :id "cc-1" :title "Claude title"
                          :directory "/tmp/claude"
                          :time_updated 1800000000000)))))
        (let ((sessions (roster--load-sessions)))
          (should (equal (mapcar (lambda (s) (plist-get s :tool)) sessions)
                         '(claude opencode)))
          (should (equal (mapcar #'roster--session-title sessions)
                         '("Claude title" "OpenCode Title"))))))))

(provide 'roster-claude-test)

;;; roster-claude-test.el ends here
