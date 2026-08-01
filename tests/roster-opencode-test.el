;;; roster-opencode-test.el --- OpenCode tests for roster -*- lexical-binding: t; -*-

;;; Commentary:

;; Internal library for roster.

;;; Code:

(require 'roster-test-helpers)

(ert-deftest roster-opencode-server-url-parses-announcement ()
  (with-temp-buffer
    (insert "warning\nopencode server listening on http://127.0.0.1:12345\n")
    (should (equal (roster--opencode-server-url (current-buffer))
                   "http://127.0.0.1:12345"))))

(ert-deftest roster-opencode-query-string-encodes-values ()
  (should (equal (roster--opencode-query-string
                  '(("roots" . "true") ("directory" . "/tmp/a b") ("cursor")))
                 "?roots=true&directory=%2Ftmp%2Fa%20b")))

(ert-deftest roster-opencode-response-decodes-utf-8-json ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string
             "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"title\":\"中文标题\"}"
             'utf-8))
    (should (equal (plist-get (cdr (roster--opencode-response (current-buffer)))
                              :title)
                   "中文标题"))))

(ert-deftest roster-opencode-session-from-api-preserves-native-fields ()
  (let ((session
         (roster--opencode-session-from-api
          '(:id "ses_root" :title "Root" :directory "/tmp/a|b"
                :projectID "proj" :time (:updated 20 :archived 10)))))
    (should (equal (plist-get session :id) "ses_root"))
    (should (equal (plist-get session :directory) "/tmp/a|b"))
    (should (equal (plist-get session :project-id) "proj"))
    (should (= (plist-get session :time-updated) 20))
    (should (= (plist-get session :time-archived) 10))))

(ert-deftest roster-opencode-load-sessions-paginates-root-api ()
  (let (calls)
    (cl-letf (((symbol-function 'roster--opencode-call-with-server)
               (lambda (function) (funcall function)))
              ((symbol-function 'roster--opencode-request)
               (lambda (_method _path parameters &optional _body)
                 (push parameters calls)
                 (if (= (length calls) 1)
                     (cons '(("x-next-cursor" . "20"))
                           '((:id "ses_2" :title "Two" :directory "/tmp/2"
                                  :projectID "p" :time (:updated 20))))
                   (cons nil
                         '((:id "ses_1" :title "One" :directory "/tmp/1"
                                :projectID "p" :time (:updated 10))))))))
      (let ((sessions (roster--opencode-load-sessions t)))
        (should (equal (mapcar #'roster--session-id sessions)
                       '("ses_2" "ses_1")))
        (should (equal (cdr (assoc "cursor" (car calls))) "20"))
        (should (equal (cdr (assoc "roots" (cadr calls))) "true"))
        (should (equal (cdr (assoc "archived" (cadr calls))) "true"))))))

(ert-deftest roster-opencode-rename-uses-native-patch ()
  (let ((session '(:id "ses_1" :directory "/tmp/a" :tool opencode)) call)
    (cl-letf (((symbol-function 'roster--opencode-api-body)
               (lambda (&rest arguments)
                 (setq call arguments)
                 '(:title "New title"))))
      (roster--opencode-rename-session session "New title")
      (should (equal call
                     '("PATCH" "/session/ses_1"
                       (("directory" . "/tmp/a"))
                       (("title" . "New title"))))))))

(ert-deftest roster-opencode-delete-uses-native-api ()
  (let ((session '(:id "ses_1" :directory "/tmp/a" :tool opencode)) call)
    (cl-letf (((symbol-function 'roster--opencode-api-body)
               (lambda (&rest arguments) (setq call arguments) t)))
      (should (roster--opencode-delete-session session))
      (should (equal call
                     '("DELETE" "/session/ses_1"
                       (("directory" . "/tmp/a"))))))))

(ert-deftest roster-opencode-archive-uses-native-timestamp ()
  (let ((session '(:id "ses_1" :directory "/tmp/a" :tool opencode)) body)
    (cl-letf (((symbol-function 'roster--opencode-update-session)
               (lambda (_session value)
                 (setq body value)
                 `(:time (:archived ,(cdr (assoc "archived"
                                                 (cdr (assoc "time" value)))))))))
      (roster--opencode-archive-session session t)
      (should (numberp (cdr (assoc "archived" (cdr (assoc "time" body)))))))))

(ert-deftest roster-opencode-unarchive-compat-clears-database-field ()
  (skip-unless (and (require 'sqlite nil t) (sqlite-available-p)))
  (let* ((path (make-temp-file "roster-opencode-" nil ".db"))
         (roster-opencode-db-path path)
         (db (sqlite-open path)))
    (unwind-protect
        (progn
          (sqlite-execute db "CREATE TABLE session (id TEXT, time_archived INTEGER)")
          (sqlite-execute db "INSERT INTO session VALUES ('ses_1', 123)")
          (sqlite-close db)
          (setq db nil)
          (roster--opencode-unarchive-compat '(:id "ses_1" :tool opencode))
          (setq db (sqlite-open path))
          (should (equal (sqlite-select
                          db "SELECT time_archived FROM session WHERE id = 'ses_1'")
                         '((nil)))))
      (when db (sqlite-close db))
      (delete-file path))))

(ert-deftest roster-opencode-move-uses-native-control-plane ()
  (let ((session '(:id "ses_1" :directory "/tmp/old" :tool opencode)) call)
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) "/tmp/new"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'roster--opencode-api-body)
               (lambda (&rest arguments) (setq call arguments) nil)))
      (should (roster--opencode-move-session session))
      (should (equal call
                     '("POST" "/experimental/control-plane/move-session" nil
                       (("sessionID" . "ses_1")
                        ("destination" ("directory" . "/tmp/new"))
                        ("moveChanges" . :json-false))))))))

(provide 'roster-opencode-test)

;;; roster-opencode-test.el ends here
