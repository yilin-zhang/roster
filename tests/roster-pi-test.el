;;; roster-pi-test.el --- pi tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-test-helpers)

;;; Session loading — pi backend

(ert-deftest roster-pi-parse-jsonl-reads-session-metadata ()
  (let ((path (make-temp-file "roster-pi-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"type\":\"session\",\"version\":3,\"id\":\"pi-session-1\",\"timestamp\":\"2026-04-05T00:00:00.000Z\",\"cwd\":\"/tmp/pi\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"entry-1\",\"parentId\":null,\"timestamp\":\"2026-04-05T00:00:01.000Z\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello pi\"}],\"timestamp\":1}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"entry-2\",\"parentId\":\"entry-1\",\"timestamp\":\"2026-04-05T00:00:02.000Z\",\"name\":\"Named pi session\"}\n"))
          (let ((meta (roster--pi-parse-jsonl path)))
            (should (equal (plist-get meta :id) "pi-session-1"))
            (should (equal (plist-get meta :cwd) "/tmp/pi"))
            (should (equal (plist-get meta :title-candidate) "Hello pi"))
            (should (equal (plist-get meta :session-name) "Named pi session"))
            (should (equal (plist-get meta :last-entry-id) "entry-2"))
            (should (> (plist-get meta :time-updated) 0))))
      (delete-file path))))

(ert-deftest roster-load-sessions-loads-pi ()
  (let* ((root (make-temp-file "roster-pi-root-" t))
         (roster-pi-dir root)
         (roster-enabled-tools '(pi))
         (sessions-dir (expand-file-name "sessions/--tmp-pi--" root))
         (path (expand-file-name "2026-04-05T00-00-00-000Z_pi-session-1.jsonl" sessions-dir)))
    (unwind-protect
        (progn
          (make-directory sessions-dir t)
          (with-temp-file path
            (insert "{\"type\":\"session\",\"version\":3,\"id\":\"pi-session-1\",\"timestamp\":\"2026-04-05T00:00:00.000Z\",\"cwd\":\"/tmp/pi\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"entry-1\",\"parentId\":null,\"timestamp\":\"2026-04-05T00:00:01.000Z\",\"message\":{\"role\":\"user\",\"content\":\"Hello pi\",\"timestamp\":1}}\n"))
          (let ((sessions (roster--load-sessions)))
            (should (= (length sessions) 1))
            (should (equal (plist-get (car sessions) :tool) 'pi))
            (should (equal (plist-get (car sessions) :title) "Hello pi"))
            (should (equal (plist-get (car sessions) :directory) "/tmp/pi"))
            (should (equal (plist-get (car sessions) :file-path) path))))
      (delete-directory root t))))

(provide 'roster-pi-test)

;;; roster-pi-test.el ends here
