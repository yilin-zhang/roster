;;; roster-codex-test.el --- Codex tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Codex app-server backend.

;;; Code:

(require 'roster-test-helpers)

(ert-deftest roster-codex-thread-from-api-prefers-native-name ()
  (let* ((thread '(:id "cx-1" :name "Native name" :preview "Preview"
                       :cwd "/tmp/codex" :updatedAt 1700000000))
         (session (roster--codex-thread-from-api thread nil)))
    (should (equal (plist-get session :id) "cx-1"))
    (should (equal (plist-get session :title) "Native name"))
    (should (equal (plist-get session :directory) "/tmp/codex"))
    (should (= (plist-get session :time-updated) 1700000000000))
    (should-not (plist-get session :time-archived))))

(ert-deftest roster-codex-thread-from-api-falls-back-to-preview ()
  (let* ((thread '(:id "cx-1" :name nil :preview "Preview title"
                       :cwd "/tmp/codex" :updatedAt 1700000000))
         (session (roster--codex-thread-from-api thread t)))
    (should (equal (plist-get session :title) "Preview title"))
    (should (roster--session-archived-p session))))

(ert-deftest roster-codex-thread-from-api-honors-legacy-sidecar ()
  (cl-letf (((symbol-function 'roster--codex-read-sidecar)
             (lambda (_id) '(("title" . "Legacy name")))))
    (let ((session (roster--codex-thread-from-api
                    '(:id "cx-1" :name "Native name" :cwd "/tmp") nil)))
      (should (equal (plist-get session :title) "Legacy name")))))

(ert-deftest roster-codex-load-state-follows-pagination ()
  (let (calls)
    (cl-letf (((symbol-function 'roster--codex-list-page)
               (lambda (archived &optional cursor)
                 (push (cons archived cursor) calls)
                 (if cursor
                     '(:data ((:id "second" :name "Second" :cwd "/b"
                                   :updatedAt 2)))
                   '(:data ((:id "first" :name "First" :cwd "/a"
                                 :updatedAt 1))
                           :nextCursor "next")))))
      (let ((sessions (roster--codex-load-state nil)))
        (should (equal (mapcar #'roster--session-id sessions)
                       '("first" "second")))
        (should (equal (nreverse calls) '((nil) (nil . "next"))))))))

(ert-deftest roster-codex-load-sessions-requests-active-and-archived ()
  (let (states)
    (cl-letf (((symbol-function 'roster--codex-call-with-app-server)
               (lambda (function) (funcall function)))
              ((symbol-function 'roster--codex-load-state)
               (lambda (archived)
                 (push archived states)
                 (list (list :id (if archived "archived" "active"))))))
      (should (equal (mapcar #'roster--session-id
                             (roster--codex-load-sessions t))
                     '("active" "archived")))
      (should (equal states '(t nil))))))

(ert-deftest roster-codex-load-sessions-skips-archived-by-default ()
  (let (states)
    (cl-letf (((symbol-function 'roster--codex-call-with-app-server)
               (lambda (function) (funcall function)))
              ((symbol-function 'roster--codex-load-state)
               (lambda (archived)
                 (push archived states)
                 nil)))
      (roster--codex-load-sessions)
      (should (equal states '(nil))))))

(ert-deftest roster-codex-rename-uses-native-api-and-removes-legacy-sidecar ()
  (let ((session '(:id "cx-1" :title "Old name" :tool codex))
        request deleted-id)
    (cl-letf (((symbol-function 'roster--codex-app-server-request)
               (lambda (method params)
                 (setq request (cons method params))))
              ((symbol-function 'roster--codex-delete-legacy-sidecar)
               (lambda (session-id) (setq deleted-id session-id))))
      (roster--codex-rename-session session "New name"))
    (should (equal request
                   '("thread/name/set"
                     ("threadId" . "cx-1")
                     ("name" . "New name"))))
    (should (equal deleted-id "cx-1"))))

(ert-deftest roster-codex-delete-uses-native-api ()
  (let (request deleted-id)
    (cl-letf (((symbol-function 'roster--codex-app-server-request)
               (lambda (method params)
                 (setq request (cons method params))))
              ((symbol-function 'roster--codex-delete-legacy-sidecar)
               (lambda (session-id) (setq deleted-id session-id))))
      (roster--codex-delete-session '(:id "cx-1")))
    (should (equal request
                   '("thread/delete" ("threadId" . "cx-1"))))
    (should (equal deleted-id "cx-1"))))

(ert-deftest roster-codex-delete-explains-active-writer ()
  (let (sidecar-deleted)
    (cl-letf (((symbol-function 'roster--codex-app-server-request)
               (lambda (_method _params)
                 (user-error
                  "Codex app-server error: thread cx-1 already has an active writer")))
              ((symbol-function 'roster--codex-delete-legacy-sidecar)
               (lambda (_session-id) (setq sidecar-deleted t))))
      (let ((err (should-error
                  (roster--codex-delete-session '(:id "cx-1"))
                  :type 'user-error)))
        (should (equal (error-message-string err)
                       (concat "Codex session cx-1 is still open; "
                               "close its Codex client, then retry")))))
    (should-not sidecar-deleted)))

(ert-deftest roster-codex-archive-uses-native-api ()
  (let (requests)
    (cl-letf (((symbol-function 'roster--codex-app-server-request)
               (lambda (method params)
                 (push (cons method params) requests))))
      (roster--codex-do-archive '(:id "cx-1") t)
      (roster--codex-do-archive '(:id "cx-1") nil))
    (should (equal (nreverse requests)
                   '(("thread/archive" ("threadId" . "cx-1"))
                     ("thread/unarchive" ("threadId" . "cx-1")))))))

(ert-deftest roster-codex-check-response-signals-api-errors ()
  (should-error
   (roster--codex-check-response
    '(:id 2 :error (:message "not found")))
   :type 'user-error))

(provide 'roster-codex-test)

;;; roster-codex-test.el ends here
