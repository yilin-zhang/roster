;;; roster-claude-test.el --- Claude Code tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-test-helpers)

;;; Session loading — Claude Code backend

(ert-deftest roster-claude-parse-jsonl-reads-user-session-metadata ()
  (let ((path (make-temp-file "roster-claude-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"slug\":\"claude-slug\",\"type\":\"system\"}\n")
            (insert (concat
                     "{\"type\":\"user\",\"cwd\":\"/tmp/claude\","
                     "\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello Claude\"}]}}\n")))
          (let ((meta (roster--claude-parse-jsonl path)))
            (should (equal (plist-get meta :slug) "claude-slug"))
            (should (equal (plist-get meta :cwd) "/tmp/claude"))
            (should (equal (plist-get meta :title-candidate) "Hello Claude"))
            (should (> (plist-get meta :time-updated) 0))))
      (delete-file path))))

(ert-deftest roster-claude-parse-jsonl-skips-system-injected-titles ()
  "Messages starting with '<' (e.g. /compact output) must not become titles."
  (let ((path (make-temp-file "roster-claude-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"slug\":\"my-slug\",\"type\":\"system\"}\n")
            (insert (concat
                     "{\"type\":\"user\",\"cwd\":\"/tmp/x\","
                     "\"message\":{\"content\":[{\"type\":\"text\","
                     "\"text\":\"<local-command-caveat>compact output</local-command-caveat>\"}]}}\n"))
            (insert (concat
                     "{\"type\":\"user\",\"cwd\":\"/tmp/x\","
                     "\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Real question\"}]}}\n")))
          (let ((meta (roster--claude-parse-jsonl path)))
            (should (equal (plist-get meta :title-candidate) "Real question"))))
      (delete-file path))))

(ert-deftest roster-claude-title-prefers-recorded-ai-title ()
  "Claude's generated title is preferred to its slug and first prompt."
  (let ((path (make-temp-file "roster-claude-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"slug\":\"old-slug\",\"type\":\"system\"}\n")
            (insert "{\"type\":\"user\",\"cwd\":\"/tmp/claude\",\"message\":{\"content\":\"First prompt\"}}\n")
            (insert "{\"type\":\"ai-title\",\"aiTitle\":\"Generated title\",\"sessionId\":\"cc-1\"}\n"))
          (let ((meta (roster--claude-parse-jsonl path)))
            (should (equal (plist-get meta :ai-title) "Generated title"))
            (should (equal (roster--claude-title meta nil) "Generated title"))))
      (delete-file path))))

(ert-deftest roster-claude-title-prefers-custom-title-to-ai-title ()
  "An explicit /rename remains authoritative over generated titles."
  (let ((meta (list :custom-title "Chosen title"
                    :ai-title "Generated title"
                    :slug "old-slug"
                    :title-candidate "First prompt")))
    (should (equal (roster--claude-title meta nil) "Chosen title"))))

(ert-deftest roster-claude-archive-preserves-sidecar-title ()
  (let* ((root (make-temp-file "roster-claude-root-" t))
         (roster-claude-dir root)
         (session '(:id "cc-1" :tool claude)))
    (unwind-protect
        (progn
          (roster--claude-write-sidecar "cc-1" "Saved title" nil)
          (roster--claude-do-archive session t)
          (let ((sidecar (roster--claude-read-sidecar "cc-1")))
            (should (equal (cdr (assoc "title" sidecar)) "Saved title"))
            (should (numberp (cdr (assoc "time_archived" sidecar))))))
      (delete-directory root t))))

(ert-deftest roster-claude-load-sessions-prefers-native-sdk ()
  (let ((roster-claude-use-agent-sdk 'auto)
        (sdk-result
         (list :available t
               :sessions
               (list (list :id "cc-1"
                           :title "SDK title"
                           :directory "/tmp/sdk"
                           :time_updated 42)))))
    (cl-letf (((symbol-function 'roster--claude-sdk-call)
               (lambda (&rest _) sdk-result))
              ((symbol-function 'roster--claude-load-sessions-from-transcripts)
               (lambda (&rest _) (error "compatibility fallback used"))))
      (let ((session (car (roster--claude-load-sessions))))
        (should (equal (plist-get session :id) "cc-1"))
        (should (equal (plist-get session :title) "SDK title"))
        (should (equal (plist-get session :directory) "/tmp/sdk"))
        (should (= (plist-get session :time-updated) 42))))))

(ert-deftest roster-claude-load-sessions-falls-back-without-sdk ()
  (let ((roster-claude-use-agent-sdk 'auto)
        (expected '((:id "cc-1" :tool claude))))
    (cl-letf (((symbol-function 'roster--claude-sdk-call) (lambda (&rest _) nil))
              ((symbol-function 'roster--claude-load-sessions-from-transcripts)
               (lambda (_include-archived) expected)))
      (should (equal (roster--claude-load-sessions t) expected)))))

(ert-deftest roster-claude-sdk-auto-falls-back-without-python ()
  (let ((roster-claude-use-agent-sdk 'auto))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (signal 'file-missing '("python3")))))
      (should-not (roster--claude-sdk-call "list")))))

(ert-deftest roster-claude-rename-prefers-native-sdk ()
  (let ((roster-claude-use-agent-sdk 'auto)
        (session '(:id "cc-1" :directory "/tmp/sdk" :tool claude))
        call cleared)
    (cl-letf (((symbol-function 'roster--claude-sdk-call)
               (lambda (&rest arguments) (setq call arguments) '(:available t)))
              ((symbol-function 'roster--claude-clear-legacy-title)
               (lambda (session-id) (setq cleared session-id)))
              ((symbol-function 'roster--claude-append-custom-title)
               (lambda (&rest _) (error "compatibility fallback used"))))
      (should (roster--claude-rename-session session "New title"))
      (should (equal call '("rename" "cc-1" "New title" "/tmp/sdk")))
      (should (equal cleared "cc-1")))))

(ert-deftest roster-load-sessions-merges-opencode-and-claude ()
  (let* ((roster-enabled-tools '(opencode claude))
         (root (make-temp-file "roster-claude-root-" t))
         (roster-claude-dir root)
         (projects-dir (expand-file-name "projects" root))
         (encoded-dir "-tmp-proj")
         (session-id "claude-session-1")
         (session-dir (expand-file-name encoded-dir projects-dir))
         (roster-claude-use-agent-sdk nil)
         (opencode-session
          (list :id "oc_1"
                :title "OpenCode Title"
                :directory "/tmp/opencode"
                :project-id "proj_1"
                :time-updated 1700000000000
                :tool 'opencode)))
    (unwind-protect
        (progn
          (make-directory session-dir t)
          (with-temp-file (expand-file-name (concat session-id ".jsonl") session-dir)
            (insert (concat
                     "{\"slug\":\"claude-slug\",\"type\":\"user\",\"cwd\":\"/tmp/claude\","
                     "\"message\":{\"content\":\"Claude title\"}}\n")))
          (cl-letf (((symbol-function 'roster--opencode-load-sessions)
                     (lambda (&optional _) (list opencode-session))))
            (let ((sessions (roster--load-sessions)))
              (should (= (length sessions) 2))
              (should (equal (mapcar (lambda (s) (plist-get s :tool)) sessions)
                             '(claude opencode)))
              (should (equal (plist-get (car sessions) :title) "claude-slug"))
              (should (equal (plist-get (cadr sessions) :title) "OpenCode Title")))))
      (delete-directory root t))))

(provide 'roster-claude-test)

;;; roster-claude-test.el ends here
